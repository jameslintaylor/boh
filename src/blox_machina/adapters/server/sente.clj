(ns blox-machina.adapters.server.sente
  (:require [blox-machina.repository-proxy :as rp]
            [blox-machina.transit :refer [transit-readers transit-writers]]
            [clojure.core.async :as a :refer [go go-loop]]
            [taoensso.sente :as sente]
            [taoensso.sente.packers.transit :as sente-transit]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [ring.util.response :refer [response]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [org.httpkit.server :as httpkit]))

(defn handle-root! [proxy req]
  (httpkit/with-channel req ch
    (a/go (let [repo (a/<! (rp/pull proxy {}))]
            (->> (count (:blocks repo))
                 (format "I am a block repository.\n I contain %d
                 non-dangling blocks.") response
                 (httpkit/send! ch))))))

(defn handle-routes! [proxy ws-get ws-post req]
  (case (:request-method req)
    :get  (case (:uri req)
            "/" (handle-root! proxy req)
            "/chsk" (ws-get req)
            (response "not a valid GET endpoint!"))
    :post (case (:uri req)
            "/chsk" (ws-post req)
            (response "not a valid POST endpoint!"))))

(defmulti handle-msg! :id)

(defmethod handle-msg! :default
  [{:keys [id]}]
  (println (format "server unhandled event: %s" id)))

(defmethod handle-msg! :upstream/pull
  [{:keys [proxy ?data ?reply-fn]}]
  (let [version (:version ?data)]
    (when-some [reply-fn ?reply-fn]
      (go (reply-fn (a/<! (rp/pull proxy version)))))))

(defmethod handle-msg! :upstream/push
  [{:keys [proxy ?data ?reply-fn]}]
  (let [diff (:diff ?data)]
    (when-some [reply-fn ?reply-fn]
      (go (reply-fn (a/<! (rp/push proxy diff)))))))

(defn push! [send-fn *connected-uids diff]
  (doseq [uid (:any @*connected-uids)]
    (send-fn uid [:downstream/push {:diff diff}])))

(defn make-handler [proxy]
  (let [packer (sente-transit/get-transit-packer
                :json {:handlers transit-writers} {:handlers transit-readers})
        chsk (sente/make-channel-socket! (get-sch-adapter) {:packer packer})
        {:keys [ch-recv send-fn connected-uids
                ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk
        push-fn (partial push! send-fn connected-uids)
        handler (partial handle-routes! proxy
                         ajax-get-or-ws-handshake-fn ajax-post-fn)]

    ;; needs a way to unregister
    (let [ch (rp/subscribe proxy {})]
      (go-loop []
        (when-some [diff (a/<! ch)]
          (push-fn diff)
          (recur))))
    
    ;; start listening for events over chsk
    (sente/start-server-chsk-router! ch-recv
                                     (comp handle-msg!
                                           #(assoc % :proxy proxy)
                                           #(assoc % :push-fn push-fn)))
    (-> handler
        wrap-keyword-params
        wrap-params)))

(defn run-sente-server [proxy port]
  (-> (make-handler proxy)
      (httpkit/run-server {:port port})))
