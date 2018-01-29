(ns blox-machina.origin
  (:require [blox-machina.blocks :as b]
            [blox-machina.util :refer [edn-packer]]
            [clojure.set :as set]
            [taoensso.sente :as sente]))

(defmulti -handle-event! :id)

(defmethod -handle-event! :default
  [{:keys [id]}]
  (println "origin unhandled" id (java.util.Date.)))

(defmethod -handle-event! :chsk/ws-ping
  [_])

(defmethod -handle-event! :client/push-blocks
  [{:keys [*chain broadcast-fn ?data ?reply-fn uid]}]
  (let [chain @*chain
        blocks (:blocks ?data)]

    (printf "client pushed %d blocks" (count blocks))

    (if (b/consecutive? chain blocks)

      (let [new-chain (swap! *chain into blocks)]
        (?reply-fn {:head (b/head new-chain)})
        (broadcast-fn [:origin/push-blocks {:blocks blocks}]
                      :excluded-uids #{uid}))

      (let [rebased-blocks (b/rebase blocks (b/head chain))
            new-chain (swap! *chain into rebased-blocks)]
        (?reply-fn {:head (b/head new-chain)
                    :rebased-blocks rebased-blocks})
        (broadcast-fn [:origin/push-blocks {:blocks rebased-blocks}]
                      :excluded-uids #{uid})))))

(defmethod -handle-event! :client/pull-blocks
  [{:keys [*chain ?data ?reply-fn]}]
  (let [chain @*chain
        base (:base ?data)]
    (?reply-fn {:head (b/head chain)
                :blocks (b/chain-since @*chain base)})))

(defn- make-chsk! [adapter]
  (let [*uid-counter (atom 0)
        opts {:packer (edn-packer {:readers b/data-readers})
              :user-id-fn (fn [_] (swap! *uid-counter inc))}]
    (sente/make-channel-socket-server! adapter opts)))

(defn- make-broadcast-fn [send-fn *connected-uids]
  (fn broadcast-fn
    ([data] (broadcast-fn data {}))
    ([data {:keys [excluded-uids]}]
     (let [all-uids (:any @*connected-uids)]
       (doseq [uid (set/difference all-uids excluded-uids)]
         (send-fn uid data))))))

(defn make-chsk-origin!
  [adapter *chain]
  (let [{:as chsk :keys [ch-recv send-fn connected-uids]} (make-chsk! adapter)
        broadcast-fn (make-broadcast-fn send-fn connected-uids)
        injected-handler (comp -handle-event!
                               #(assoc % :*chain *chain)
                               #(assoc % :broadcast-fn broadcast-fn))]
    (sente/start-server-chsk-router! ch-recv injected-handler)
    (assoc chsk :broadcast-fn broadcast-fn)))
