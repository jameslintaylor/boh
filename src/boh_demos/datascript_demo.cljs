(ns boh-demos.datascript-demo
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
            [cljs.core.async :as a]
            [datascript.core :as d]
            [datascript.transit :as dt])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defonce *repo (rr/make-ref))
(defonce *state
  (atom {:me :anon
         :branch "master"
         :detached nil
         :upstreams {"origin"    {:host "127.0.0.1:3000"}
                     "jamesl.in" {:host "jamesl.in:3000"}}}))

(def read-transit (partial dt/read-transit-str))
(def write-transit (partial dt/write-transit-str))

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

(rum/defc image-section [state prepo]
  (let [{:keys [me branch detached]} state
        h (or detached (get-in prepo [:heads (u/branch-kw branch)]))
        image (get-in prepo [:frames h :image])]
    (println (count (d/q '[:find ?e ?a
                           :where [?e :age ?a]]
                         image)))))

(rum/defc root
  < rum/reactive
  [prepo]
  (let [state (rum/react *state)]
    [:div#container
     (image-section state prepo)
     (branch-section state prepo)
     (upstream-section state)]))

(defn rf
  ([] (d/empty-db))
  ([db] db)
  ([db tx-data]
   (:db-after (d/with db tx-data))))

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
