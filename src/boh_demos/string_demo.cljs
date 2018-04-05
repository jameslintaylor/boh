(ns boh-demos.string-demo
  (:require [rum.core :as rum]
            [boh-demos.handlers :as h]
            [boh-demos.components :as c]
            [boh.repository :as r]
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
(defonce *state
  (atom {:me :no1
         :branch "master"
         :detached nil
         :upstreams {"origin"    {:host "127.0.0.1:3000"}
                     "jamesl.in" {:host "jamesl.in:3000"}}}))

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
    (swap! *state assoc-in [:upstreams name :unsync] "diapers")))

(defn toggle-sync! [name host]
  (let [unsync (get-in @*state [:upstreams name :unsync])]
    (if (nil? unsync)
      (sync! name host)
      (swap! *state update-in [:upstreams name] dissoc :unsync))))

(defn commit! [& events]
  (let [branch (:branch @*state)]
    (apply rr/commit! *repo (u/branch-kw branch)
           (map write-transit events))))

(def color-pool ["#282a2e" "#a54242" "#8c9440" "#de935f"
                 "#5f819d" "#85678f" "#5e8d87" "#707880"])

(rum/defc room-editor [me image]
  (let [room (:room image)]
    [:#room-editor
     [:input
      {:value (name me)
       :on-key-down
       (h/handler h/wrap-keycode
                  (filter (comp (partial = 13) :keycode))
                  h/wrap-target-value
                  (fn [{:keys [target-value]}]
                    (commit! [:join (keyword target-value)
                              0 (rand-nth color-pool)])))
       :on-change
       (h/handler h/wrap-target-value
                  (fn [{:keys [target-value]}]
                    (swap! *state assoc :me (keyword target-value))))}]
     (for [[user _] room]
       (-> [:ul.user (name user)]
           (c/attr :key user)
           (c/on-click (partial swap! *state update :me user))))]))

(defn cursor-character [text pos]
  (let [c (get text pos)]
    (case c
      nil " "
      "\r" "↙"
      c)))

(defn cursor-separated-text [image]
  (let [{:keys [text room]} image]
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

(defn handle-key-down! [me pos e]
  (let [keycode (c/keycode e)]
    (case keycode
      8 (commit! [:delete (dec pos) pos]
                 [:move me (dec pos)])
      13 (do (c/prevent-default e)
             (commit! [:insert "\r\n" pos]
                      [:move me (inc pos)]))
      37 (commit! [:move me (dec pos)])
      39 (commit! [:move me (inc pos)])
      nil)))

(defn handle-key-press! [me pos e]
  (c/prevent-default e)
  (let [char (char (c/keycode e))]
    (commit! [:insert char pos]
             [:move me (inc pos)])))

(defn cursor [c]
  (let [[k color text] c]
    [:span.cursor {:style {:background-color color}}
     text
     [:.cursor-popup
      [:span {:style {:color color}} (name k)]]]))

(rum/defc text-editor [me image]
  (let [pos (get-in image [:room me :pos])]
    [:div#text-section.lowered
     {:tab-index 1
      :on-key-down (partial handle-key-down! me pos)
      :on-key-press (partial handle-key-press! me pos)}
     (for [x (cursor-separated-text image)]
       (if (string? x)
         [:span.subs x]
         (cursor x)))]))

(rum/defc image-section [state prepo]
  (let [{:keys [me branch detached]} state
        h (or detached (get-in prepo [:heads (u/branch-kw branch)]))
        image (get-in prepo [:frames h :image])]
    (-> [:section#image-section
         (room-editor me image)
         (text-editor me image)]
        (c/class (when (some? detached) "read-only"))
        (c/attr :disabled (some? detached)))))

(defn upstream-control [upstream]
  (let [[name {:keys [host unsync]}] upstream]
    [:div.upstream {:key name}
     [:span.upstream-name name]
     [:span "➩"]
     [:span.upstream-url
      (-> [:input]
          (c/attr :value host)
          (c/on-change-set *state [:upstreams name :host]))]
     (-> [:button "⇂"] (c/on-click (partial pull! name host)))
     (-> [:button "↿"] (c/on-click (partial push! name host)))
     (-> [:button "⟲"]
         (c/attr :class (if (some? unsync) ["active"] []))
         (c/on-click (partial toggle-sync! name host)))
     (-> [:button "✕"]
         (c/on-click
          (fn []
            (swap! *state update :upstreams dissoc name))))]))

(rum/defcs upstream-section
  < (rum/local false ::open?)
  [cs state]
  (let [*open? (::open? cs)
        open? @*open?
        upstreams (:upstreams state)
        add-upstream-fn (fn [e]
                          (let [name (c/target-value e)]
                            (swap! *state assoc-in [:upstreams name] {})))]
    [:section#upstream-section
     {:class (if open? ["open"] ["closed"])}
     (-> [:button#toggle-drawer-button
          (if open? "◤" "◢")]
         (c/on-click (partial swap! *open? not)))
     #_[:button#toggle-drawer-button "⎇"]
     [:div#upstream-drawer
      [:div#upstream-list
       (map upstream-control upstreams)]
      [:div#new-upstream
       (-> [:input] (c/on-enter add-upstream-fn))
       (-> [:button "➩"] (c/on-click add-upstream-fn))]]]))

(rum/defc version-list
  ;; this does equality on an entire prepo, if performance becomes a
  ;; concern, should probably just be comparing heads...
  < rum/static
  [branch prepo n]
  [:#version-list
   (for [h (into [] (comp (take n)
                          (map :hash))
                 (r/-traverse (:frames prepo)
                              (get-in prepo [:heads (u/branch-kw branch)])))]
     (-> [:li {:key h}
          (subs (name h) 0 6)]
         (c/on-click ;; this should decorate with value maybe...
          (fn []
            (rr/swap-step! *repo :step
                           r/upsert-branch
                           (u/branch-kw (name h))
                           h)
            (swap! *state assoc :branch (name h))))
         (c/on-mouse-enter (partial swap! *state assoc :detached h))
         (c/on-mouse-out (partial swap! *state assoc :detached nil))))])

(rum/defc branch-section
  [state prepo]
  (let [{:keys [branch detached]} state
        {:keys [heads frames]} prepo]
    [:section#branch-section
     [:#branch-switcher.raised
      {:class (when (some? detached) "read-only")}
      [:input {:value branch}]
      [:#branch-list
       (for [[b h] heads]
         (let [n (name b)]
           (when-not (= n state)
             (-> [:ul.branch n]
                 (c/on-click (partial swap! *state assoc :branch n))))))]]
     (version-list branch prepo 10)]))

(rum/defc root
  < rum/reactive
  [prepo]
  (let [state (rum/react *state)]
    [:div#container
     (image-section state prepo)
     (branch-section state prepo)
     (upstream-section state)]))

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

(defonce initial-mount
  (let [p-ch (p/projection! *repo (map read-transit) rf)]
    (go-loop []
      (when-some [prepo (a/<! p-ch)]
        (rum/mount (root prepo)
                   (. js/document (getElementById "app")))
        (recur)))
    ;; initial mount
    (a/put! p-ch {:heads {} :projections {}})))

(defn on-js-reload []
  (println "js reloaded"))
