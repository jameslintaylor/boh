(ns blox-machina-demos.datascript-demo
  (:require [blox-machina-demos.components :as c]
            [blox-machina.blocks :as b]
            [blox-machina.client :as client]
            [blox-machina.projection :as p]
            [cljs.tools.reader.edn :as edn]
            [datascript.core :as d]
            [rum.core :as rum]))

(defn make-pair! [] (client/make-pair-client! "origin" {:host "localhost:3001"
                                                        :readers d/data-readers}))

(def builder
  (p/reducing-builder (d/empty-db)
    (fn [db tx-data]
      (:db-after (d/with db tx-data)))))

(let [[*origin *local *disconnect<- *disconnect->] (make-pair!)]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *disconnect<- *disconnect<-)
  (def *disconnect-> *disconnect->)
  (def *projection (p/create-projection-client! *chain-origin *chain-local builder))
  ;; datascript expects this metadata for listening to conn
  (alter-meta! *projection assoc :listeners (atom {}))
  (p/setup-reflection! *projection *chain-local
                       (fn [*p reflect!]
                         (d/listen! *p (fn [{:keys [tx-data]}]
                                         (reflect! tx-data))))))

(defn add-name [name]
  (d/transact! *projection [[:db/add -1 :name name]]))

(rum/defc input-name-field []
  [:input
   {:style {:color "white"
            :background-color "black"
            :border-style "solid"
            :border-color "white"
            :border-width 1}
    :on-key-up (fn [e]
                 (when (= (.-keyCode e) 13)
                   (add-name (.. e -target -value))
                   (set! (.. e -target -value) "")))}])

(rum/defc name-list < rum/reactive []
  (let [db (rum/react *projection)]
    [:div#name-list
     (for [[entity name] (d/q '[:find ?e ?n :where [?e :name ?n]] db)]
       [:div
        {:style {:cursor "pointer"}
         :on-click (fn [e] (d/transact! *projection [[:db.fn/retractEntity entity]]))}
        name])]))

(rum/defc editable-field < rum/reactive []
  (let [db (rum/react *projection)]
    [:div
     [:textarea
      {:on-change (fn [e]
                    (let [value (.. e -target -value)]
                      (d/transact! *projection [[:db/add 1 :value value]])))}]
     [:div
      (first (first (d/q '[:find ?v :where [1 :value ?v]] db)))]]))

(rum/defc datascript-demo []
  [:div
   (c/network-toggles *disconnect<- *disconnect->)
   (c/header *chain-origin *chain-local)
   (input-name-field)
   (name-list)
   (editable-field)])

(rum/mount (datascript-demo)
           (. js/document (getElementById "app")))
