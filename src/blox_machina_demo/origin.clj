(ns blox-machina-demo.origin
  (:require [blox-machina.blocks :as b]
            [blox-machina.origin :as origin]
            [com.stuartsierra.component :as component]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(defrecord Origin []
  component/Lifecycle

  (start [this]
    (let [*chain (atom (b/create-chain :gen))
          chsk (origin/make-chsk-origin! (get-sch-adapter) *chain)]
      (println "starting origin with an empty chain")
      (assoc this
             :*chain *chain
             :chsk chsk)))

  (stop [this]
    (dissoc this :*chain :chsk)))

(defn make-origin [opts]
  (map->Origin opts))
