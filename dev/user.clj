(ns user
  (:require [figwheel-sidecar.repl-api :as f]
            [blox-machina.repository :as r]
            [blox-machina.repository-reference :as rr]
            [blox-machina.adapters.server.httpkit :refer [run-httpkit-server]]
            [blox-machina.adapters.server.sente :refer [run-sente-server]]))

(defonce a (rr/make-ref))

(defonce stop! (constantly nil))

(defn start! []
  (stop!)
  (println "starting server on port 3000")
  (def stop! (run-sente-server (rr/as-proxy a) 3000)))

(start!)
