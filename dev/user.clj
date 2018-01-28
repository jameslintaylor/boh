(ns user
  (:require [com.stuartsierra.component :as component]
            [figwheel-sidecar.repl-api :as f]
            [blox-machina-demo.system :as system]))

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
