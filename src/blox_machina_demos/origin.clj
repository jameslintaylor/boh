(ns blox-machina-demos.origin
  (:require [blox-machina.blocks :as b]
            [blox-machina.origin :as origin]
            [com.stuartsierra.component :as component]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [datascript.core :as d]))

(defrecord Origin []
  component/Lifecycle

  (start [this]
    (let [*chain (atom (b/create-chain :gen))
          chsk (origin/make-chsk-origin! (get-sch-adapter) *chain
                                         {:readers d/data-readers})]
      (println (format "starting origin with a %d-block chain" (count @*chain)))
      (assoc this
             :*chain *chain
             :chsk chsk)))

  (stop [this]
    (dissoc this :*chain :chsk)))

(defn make-origin [opts]
  (map->Origin opts))
