(ns boh.adapters.client.sente
  (:require [taoensso.sente :as sente]
            [taoensso.sente.packers.transit :as sente-transit]
            [boh.repository-proxy :refer [RepositoryProxy]]
            [boh.transit :refer [transit-readers transit-writers]]
            [cljs.core.async :as a])
  (:require-macros [boh.util :refer [do-with]]
                   [cljs.core.async.macros :refer [go go-loop]]))

(defn watch-ch [iref key]
  (do-with [ch (a/chan)]
           (add-watch iref key (fn [_ _ _ new]
                                 (a/put! ch new)))))

;; this should probably employ a pub/sub model.
(defn make-queue-send-fn [send-fn state-atom]
  (let [state-ch (watch-ch state-atom ::queue)]
    (fn [event]
      (do-with [response-ch (a/chan)]
        (if (:open? @state-atom)
          (send-fn event 1000 (partial a/put! response-ch))
          (go-loop [] 
            (let [new-state (a/<! state-ch)]
              (if (:open? new-state)
                (send-fn event 1000 (partial a/put! response-ch))
                (recur)))))))))

(defn sente-proxy [host]
  (let [packer (sente-transit/get-transit-packer :json
                                                 {:handlers transit-writers}
                                                 {:handlers transit-readers})
        chsk (sente/make-channel-socket-client! "/chsk" {:host host :packer packer})
        {:keys [ch-recv send-fn state]} chsk
        queue-send! (make-queue-send-fn send-fn state)]
    (reify RepositoryProxy
      (pull [_ version]
        (queue-send! [:upstream/pull {:version version}]))
      (push [_ diff]
        (queue-send! [:upstream/push {:diff diff}]))
      (subscribe [_ version]
        (let [xform (comp (filter (comp (partial = :chsk/recv) :id))
                          (map :?data)
                          (filter (comp (partial = :downstream/push) first))
                          (map second)
                          (map :diff))]
          (do-with [ch (a/chan)]
                   (a/pipeline 1 ch xform ch-recv)))))))
