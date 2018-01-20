(ns user
  (:require [blox-machina.blocks :as b]
            [com.stuartsierra.component :as component]
            [figwheel-sidecar.repl-api :as f]
            system))

(defonce *chain (atom (b/chain-contents :gen 1 2)))
(defonce system (system/make-system *chain))

(defn start! []
  (alter-var-root #'system component/start))

(defn stop! []
  (alter-var-root #'system component/stop))

(defn reset! []
  (stop!)
  (alter-var-root #'system (constantly (system/make-system *chain))))

(defn restart! []
  (stop!)
  (start!))
