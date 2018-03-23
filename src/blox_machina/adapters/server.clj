(ns blox-machina.adapters.server
  (:require [blox-machina.chain-proxy :as p]
            [com.stuartsierra.component :as component]
            [org.httpkit.server :as httpkit]
            [ring.util.response :as r]
            [taoensso.sente :as sente]))

(defn- handle-pull [proxy req]
  (let [base (get-in req [:something :base])]
    (r/response (p/pull-result proxy base))))

(defn- handle-push [proxy req]
  (let [chain (get-in req [:something :chain])]
    (r/response (p/push-result proxy chain))))

(defrecord HTTPAdapter [proxy]

  component/Lifecycle
  (start [this]
    (assoc this
           :pull-handler (partial handle-pull proxy)
           :push-handler (partial handle-push proxy)))

  (stop [this]
    (dissoc this :pull-handler :push-handler)))

;; SENTE ADAPTER

(defmulti -handle-event! :id)

(defmethod -handle-event! :default
  [{:keys [id]}]
  (println "senver unhandled" id (java.util.Date.)))

(defmethod -handle-event! :chsk/ws-ping
  [_])

(defn as-push-msg [push-result]
  [:server/push {:chain (:forward-chain push-result)}])

(defmethod -handle-event! :client/push
  [{:keys [proxy broadcast-fn ?data ?reply-fn uid]}]
  (let [chain (:chain ?data)]
    (let [result (p/push-result proxy chain)]
      (?reply-fn result)
      (when (:success? result)
        (broadcast-fn (as-push-msg result))))))

(defmethod -handle-event! :client/pull
  [{:keys [proxy ?data ?reply-fn]}]
  (let [base (:base ?data)]
    (?reply-fn (p/pull-result proxy base))))

(defrecord SenteAdapter [proxy chsk]

  component/Lifecycle
  (start [this]
    (sente/start-server-chsk-router! (:ch-recv chsk) -handle-event!))

  (stop [this]))
