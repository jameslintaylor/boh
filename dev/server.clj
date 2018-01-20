(ns server
  (:require [org.httpkit.server :as httpkit]
            [com.stuartsierra.component :as component]))

(defrecord Server [app port]
  component/Lifecycle

  (start [this]
    (println "dutifully starting server on port:" port)
    (assoc this :server (httpkit/run-server app {:port port})))

  (stop [this]
    (when-let [stop! (:server this)]
      (println "gracefully shutting down server...")
      (stop! :timeout 100))
    (dissoc this :server)))

(defn make-server [opts]
  (map->Server opts))
