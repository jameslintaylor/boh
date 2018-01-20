(ns blox-machina.origin
  (:require [blox-machina.blocks :as b]
            [blox-machina.util :as util]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(defn make-chsk! []
  (sente/make-channel-socket-server! (get-sch-adapter) {:packer util/packer}))

(defmulti handle-msg! :id)

(defmethod handle-msg! :default
  [{:keys [id]}]
  (println "origin unhandled" id (java.util.Date.)))

(defmethod handle-msg! :chsk/ws-ping
  [_])

(defmethod handle-msg! :client/pull
  [{:keys [*chain data ?reply-fn]}]
  (let [base (:base data)
        tail (b/tail @*chain base)]
    (?reply-fn tail)))

(defmethod handle-msg! :client/push
  [{:keys [*chain ?data ?reply-fn]}]
  (let [blocks (:blocks ?data)
        base (:prev-block (first blocks))]
    (println "client pushed with base" base)
    (if-not (= base (:hash (last @*chain)))
      (?reply-fn :failed)
      (do
        (swap! *chain into blocks)
        (?reply-fn :success)))))

(defn serve-origin!
  [*chain]
  (let [{:keys [ch-recv send-fn connected-uids
                ajax-get-or-ws-handshake-fn ajax-post-fn]} (make-chsk!)
        handler (comp handle-msg! #(assoc % :*chain *chain))]

    (sente/start-server-chsk-router! ch-recv handler)

    ;; testing
    (def *connected-uids connected-uids)
    (defn broadcast [msg]
      (doseq [uid (:any @connected-uids)]
        (println "broadcasting to" uid)
        (send-fn uid msg)))

    [ajax-get-or-ws-handshake-fn ajax-post-fn]))
