(ns blox-machina-demo.server
  (:require [blox-machina.origin :as origin]
            [compojure.core :as cpj]
            [com.stuartsierra.component :as component]
            [org.httpkit.server :as httpkit]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]))

(defn make-handler [chsk-origin]
  (let [{:keys [ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk-origin
        routes (cpj/defroutes routes
                 (cpj/GET "/" req "one root to find them")
                 (cpj/GET "/origin" req (ajax-get-or-ws-handshake-fn req))
                 (cpj/POST "/origin" req (ajax-post-fn req)))]
    (-> routes
        wrap-keyword-params
        wrap-params)))

(defrecord Server [origin handler port]
  component/Lifecycle

  (start [this]
    (println "starting server on port" port)
    (assoc this :server (httpkit/run-server (make-handler (:chsk origin)) {:port port})))

  (stop [this]
    (when-let [stop! (:server this)]
      (println "gracefully shutting down server...")
      (stop! :timeout 100))
    (dissoc this :server)))

(defn make-server [opts]
  (map->Server opts))
