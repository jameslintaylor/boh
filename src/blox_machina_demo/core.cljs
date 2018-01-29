(ns blox-machina-demo.core
  (:require [blox-machina.client :as client]
            [rum.core :as rum]
            [blox-machina.blocks :as b]))

(enable-console-print!)

(println "This text is printed from src/blox-machina/core.cljs. Go ahead and edit it and see reloading in action.")

(let [[*origin *local] (client/make-pair-client! "origin" {:host "localhost:3001"})]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *chain (rum/derived-atom [*chain-origin *chain-local] ::full-chain
                (fn [chain-origin chain-local]
                  (into chain-origin chain-local)))))

(rum/defc hello-world < rum/reactive []
  (let [head (b/head (rum/react *chain))]
    [:div
     [:h2 (str "head: " (subs (pr-str head) 1 8))]]))

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
)
