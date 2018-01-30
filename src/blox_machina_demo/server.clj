(ns blox-machina-demo.server
  (:require [blox-machina.origin :as origin]
            [com.stuartsierra.component :as component]
            [compojure.core :as cpj]
            [org.httpkit.server :as httpkit]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [rum.core :as rum]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(rum/defc list-chain [chain]
  [:div
   [:div [:font {:color "gray"} "-------- "(count chain) " blocks --------"]]
   (for [block chain]
     [:div
      [:font {:color "gray"} (:hash block)]
      " "
      [:font {:color "orange"} (pr-str (:data block))]])])

(defn make-handler [*chain chsk-origin]
  (let [{:keys [ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk-origin
        routes (cpj/defroutes routes
                 (cpj/GET "/" req "one root to find them")
                 (cpj/GET "/blocks" req (rum/render-html (list-chain @*chain)))
                 (cpj/GET "/origin" req (ajax-get-or-ws-handshake-fn req))
                 (cpj/POST "/origin" req (ajax-post-fn req)))]
    (-> routes
        wrap-keyword-params
        wrap-params)))

(defrecord Server [origin handler port]
  component/Lifecycle

  (start [this]
    (println "starting server on port" port)
    (assoc this :server (httpkit/run-server (make-handler (:*chain origin) (:chsk origin)) {:port port})))

  (stop [this]
    (when-let [stop! (:server this)]
      (println "gracefully shutting down server...")
      (stop! :timeout 100))
    (dissoc this :server)))

(defn make-server [opts]
  (map->Server opts))
