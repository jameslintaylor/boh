(ns blox-machina-demo.system
  (:require [com.stuartsierra.component :as component]
            [blox-machina-demo.server :as server]
            [blox-machina-demo.origin :as origin]))

(defn make-system []
  (component/system-map
   :origin (origin/make-origin {})
   :server (component/using (server/make-server {:port 3001}) [:origin])))
