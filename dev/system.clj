(ns system
  (:require [blox-machina.blocks :as blocks]
            [blox-machina.origin :as origin]
            [com.stuartsierra.component :as component]
            [compojure.core :as cpj]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            server))

(defn make-app [*chain]
  (let [[origin-get origin-post] (origin/serve-origin! *chain)
        routes (cpj/defroutes routes
                 (cpj/GET "/" req "one root to find them")
                 (cpj/GET "/origin" req (origin-get req))
                 (cpj/POST "/origin" req (origin-post req)))]
    (-> routes
        wrap-keyword-params
        wrap-params)))

(defn make-system [*chain]
  (component/system-map
   :server (server/make-server {:app (make-app *chain) :port 3001})))
