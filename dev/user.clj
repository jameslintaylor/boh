(ns user
  (:require [blox-machina-demo.system :as system]
            [blox-machina.blocks :as b]
            [com.stuartsierra.component :as component]
            [figwheel-sidecar.repl-api :as f]))

(defonce system (system/make-system))

(defn start! []
  (alter-var-root #'system component/start))

(defn stop! []
  (alter-var-root #'system component/stop))

(defn reset! []
  (stop!)
  (alter-var-root #'system (constantly (system/make-system))))

(defn restart! []
  (stop!)
  (start!))

(defn link-data-and-push! [& data]
  (let [head (b/head @(:*chain (:origin system)))
        new-blocks (apply b/create-chain head data)
        new-head (b/head new-blocks)]
    (swap! (:*chain (:origin system)) b/link new-blocks)
    ((get-in system [:origin :chsk :broadcast-fn])
     [:origin/push-blocks {:head new-head
                           :blocks new-blocks}])))
