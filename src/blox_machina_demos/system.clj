(ns blox-machina-demos.system
  (:require [com.stuartsierra.component :as component]
            [blox-machina-demos.server :as server]
            [blox-machina-demos.origin :as origin]))

(defn make-system []
  (component/system-map
   :origin (origin/make-origin {})
   :server (component/using (server/make-server {:port 3001}) [:origin])))
