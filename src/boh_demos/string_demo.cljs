(ns boh-demos.string-demo
  (:require [rum.core :as rum]
            [boh-demos.components :as c]
            [boh.repository-reference :as rr]
            [boh.upstream :as u]
            [boh.projection :as p]
            [boh.adapters.client.sente :refer [sente-proxy]]
            [cognitect.transit :as transit]
            [clojure.string :as s]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defonce *repo (rr/make-ref))
(defonce *branch (atom "master"))
(defonce *me (atom nil))
(defonce *upstreams (atom {"origin" {:host "127.0.0.1:3000"}}))

(def read-transit (partial transit/read (transit/reader :json)))
(def write-transit (partial transit/write (transit/writer :json)))

(defn pull! [name host]
  (u/pull-rebase-upstream! *repo (sente-proxy host) name))

(defn push! [name host]
  (u/push-revert-upstream! *repo (sente-proxy host) name))

(defn sync! [name host]
  (let [proxy (sente-proxy host)]
    (u/auto-pull-rebase-upstream! *repo proxy name)
    (u/auto-push-revert-upstream! *repo proxy name)
    (swap! *upstreams assoc-in [name :unsync] "diapers")))

(defn toggle-sync! [name host]
  (let [unsync (get-in @*upstreams [name :unsync])]
    (if (nil? unsync)
      (sync! name host)
      (swap! *upstreams update name dissoc :unsync))))

(defn commit! [& events]
  (apply rr/commit! *repo (u/branch-kw @*branch) (map write-transit events)))

(defn track-remote! [url name]
  (let [proxy (sente-proxy url)]
    (u/pull-upstream! *repo proxy name)
    (u/auto-pull-rebase-upstream! *repo proxy name)
    (u/auto-push-revert-upstream! *repo proxy name)))

(rum/defc cursor-input
  < rum/reactive
  [*a path]
  (let [value (get-in (rum/react *a) path)]
    [:input
     {:value (get-in @*a path)
      :on-change (fn [e]
                   (let [new (c/target-value e)]
                     (swap! *a assoc-in path new)))}]))

(defn upstream-control [upstream]
  (let [[name {:keys [host unsync]}] upstream]
    [:div.upstream {:keys name}
     [:span.upstream-name name]
     [:span "➩"]
     [:span.upstream-url (cursor-input *upstreams [name :host])]
     (-> [:button "⇂"] (c/on-click (partial pull! name host)))
     (-> [:button "↿"] (c/on-click (partial push! name host)))
     (-> [:button "⟲"]
         (c/attr :class (if (some? unsync) ["active"] [])) 
         (c/on-click (partial toggle-sync! name host)))
     (-> [:button "✕"]
         (c/on-click
          (fn []
            (swap! *upstreams dissoc name))))]))

(rum/defc upstream-area < rum/reactive []
  (let [upstreams (rum/react *upstreams)
        add-upstream-fn (fn [e]
                          (let [name (c/target-value e)]
                            (swap! *upstreams assoc name {})))]
    [:div#upstream-area
     [:div#upstream-list
      (map upstream-control upstreams)]
     [:div#new-upstream
      (-> [:input] (c/on-enter add-upstream-fn))
      (-> [:button "➩"] (c/on-click add-upstream-fn))]]))

(rum/defc branches-area [heads]
  [:div#branches-area
   [:input]
   (for [[branch hash] heads]
     (-> [:div.branch (name branch)]
         (c/on-click (partial reset! *branch (name branch)))))])

(rum/defc me-control < rum/reactive [room]
  (let [me (rum/react *me)
        my-color (or (get-in room [(keyword me) :color]) "gray")]
    [:div#me
     (-> [:input]
         (c/style :color my-color)
         (c/on-change-set *me [])
         (c/on-enter
          (fn [e]
            (let [keycode (c/keycode e)
                  name (c/target-value e)]
              (commit! [:join name 0])))))
     [:span#color-selector
      {:style {:color my-color}}
      "*"]]))

(rum/defc room-list [room]
  [:div#users-list
   (for [[k {:keys [color]}] room]
     [:div.user
      {:key k
       :style {:color color}}
      (name k)])])

(rum/defc room-area [room]
  [:div#room-area
   (me-control room)
   (room-list room)])

(defn cursor-character [text pos]
  (let [c (get text pos)]
    (case c
      nil " "
      "\r" "↙"
      c)))

(defn cursor-separated-text [s]
  (let [{:keys [text room]} s]
    (let [sorted-cursors (sort #(< (-> %1 second :pos)
                                   (-> %2 second :pos))
                               room)]
      (loop [[[k {:keys [pos color]}] & rest] sorted-cursors
             result []
             offset 0]
        (if (nil? k)
          (conj result (subs (or text "") offset))
          (recur rest
                 (conj result
                       (subs text offset pos)
                       [k color (cursor-character text pos)])
                 (+ offset pos 1)))))))

(defn handle-key-down! [pos e]
  (let [keycode (c/keycode e)]
    (case keycode
      8 (commit! [:delete (dec pos) pos]
                 [:move :a (dec pos)])
      13 (do (c/prevent-default e)
             (commit! [:insert "\r\n" pos]
                      [:move :a (inc pos)]))
      37 (commit! [:move :a (dec pos)])
      39 (commit! [:move :a (inc pos)])
      (prn "key down! " keycode pos))))

(defn handle-key-press! [pos e]
  (c/prevent-default e)
  (let [char (char (c/keycode e))]
    (commit! [:insert char pos]
             [:move :a (inc pos)])))

(defn cursor [c]
  (let [[k color text] c]
    [:span#cursor {:style {:background-color color}}
     text
     [:.cursor-popup
      [:span {:style {:color color}} (name k)]]]))

(rum/defc text-area [s]
  [:div#text-area
   {:tab-index 1
    :on-key-down (partial handle-key-down! (get-in s [:room :a :pos]))
    :on-key-press (partial handle-key-press! (get-in s [:room :a :pos]))}
   (for [el (cursor-separated-text s)]
     (if (string? el)
       [:span.subs el]
       (cursor el)))])

(rum/defc root
  < rum/reactive
  [prepo]
  (let [branch (rum/react *branch)
        heads (:heads prepo)
        head (get heads (u/branch-kw branch))
        s (get-in prepo [:frames head :image])]
    [:div#container
     (upstream-area)
     (branches-area heads)
     (text-area s)
     (room-area (:room s))]))

(defmulti apply-event (comp first second))

(defmethod apply-event :insert
  [[s [_ text pos]]]
  (update s :text
          (fn [t]
            (str (subs t 0 pos) text (subs t pos)))))

(defmethod apply-event :delete
  [[s [_ start end]]]
  (update s :text
          (fn [t]
            (str (subs t 0 start) (subs t end)))))

(defmethod apply-event :join
  [[s [_ nick pos color]]]
  (update s :room assoc nick {:pos pos :color color}))

(defmethod apply-event :part
  [[s [_ nick]]]
  (update s :room dissoc nick))

(defn clamp [x mn mx]
  (max (min x mx) mn))

(defn new-pos [text prev-pos suggested-pos]
  (let [clamped-pos (clamp suggested-pos 0 (count text))]
    (if-not (= "\n" (get text clamped-pos))
      clamped-pos
      (let [step (if (< prev-pos suggested-pos) 1 -1)]
        (recur text prev-pos (+ suggested-pos step))))))

(defmethod apply-event :move
  [[s [_ nick pos]]]
  (update-in s [:room nick :pos] #(new-pos (:text s) % pos)))

(defn rf
  ([] {:text "" :room {}})
  ([s] s)
  ([s event]
   (apply-event [s event])))

(let [p-ch (p/projection! *repo (map read-transit) rf)]
  (go-loop []
    (when-some [prepo (a/<! p-ch)]
      (println (count (:frames prepo)) " frames")
      (rum/mount (root prepo)
                 (. js/document (getElementById "app")))
      (recur)))
  ;; initial mount
  (a/put! p-ch {:heads {} :projections {}}))

(defn on-js-reload []
  (println "js reload"))
