(ns blox-machina.origin
  (:require [blox-machina.blocks :as b]
            [blox-machina.util :refer [edn-packer]]
            [taoensso.sente :as sente]))

(defmulti -handle-event! :id)

(defmethod -handle-event! :default
  [{:keys [id]}]
  (println "origin unhandled" id (java.util.Date.)))

(defmethod -handle-event! :chsk/ws-ping
  [_])

(defmethod -handle-event! :client/push-blocks
  [{:keys [*chain ?data ?reply-fn]}]
  (let [chain @*chain
        blocks (:blocks ?data)]

    (printf "client pushed %d blocks" (count blocks))

    (if (b/consecutive? chain blocks)

      (let [new-chain (swap! *chain into blocks)]
        (?reply-fn {:head (b/head new-chain)}))

      (let [rebased-blocks (b/rebase blocks (b/head chain))
            new-chain (swap! *chain into rebased-blocks)]
        (?reply-fn {:head (b/head new-chain)
                    :rebased-blocks rebased-blocks})))))

(defmethod -handle-event! :client/pull-blocks
  [{:keys [*chain data ?reply-fn]}]
  (let [chain @*chain
        base (:base data)]
    (?reply-fn {:blocks (b/chain-since @*chain base)})))

(defn- make-chsk! [adapter]
  (let [opts {:packer (edn-packer {:readers b/data-readers})}]
    (sente/make-channel-socket-server! adapter opts)))

(defn make-chsk-origin!
  [adapter *chain]
  (let [{:as chsk :keys [ch-recv]} (make-chsk! adapter)
        injected-handler (comp -handle-event! #(assoc % :*chain *chain))]
    (sente/start-server-chsk-router! ch-recv injected-handler)
    (assoc chsk :*chain *chain)))
