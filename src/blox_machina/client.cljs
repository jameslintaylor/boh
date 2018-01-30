(ns blox-machina.client
  (:require [blox-machina.util :refer [edn-packer callback-chan! pipe]]
            [cljs.core.async :as a]
            [taoensso.sente :as sente]
            [blox-machina.blocks :as b])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defmulti -handle-event! :id)

(defmethod -handle-event! :default
  [{:keys [id]}]
  (println "client unhandled event" id))

(defmethod -handle-event! :origin/push-blocks
  [{:keys [*chain-origin *chain-local ?data]}]
  (let [chain-origin @*chain-origin
        {:keys [head blocks]} ?data]
    (if (b/consecutive? chain-origin blocks)
      (do
        (swap! *chain-origin b/link blocks)
        (swap! *chain-local b/rebase head))
      (println "server pushed dangling blocks, should probably pull"))))

(defmulti -handle-msg! :id)

(defmethod -handle-msg! :default
  [{:keys [id]}]
  (println "client unhandled message" id))

(defmethod -handle-msg! :chsk/ws-ping
  [_])

(defmethod -handle-msg! :chsk/recv
  [{:as msg :keys [*chain-origin *chain-local *disconnect<- ?data]}]
  (when-not @*disconnect<-
    (-handle-event! {:id (first ?data)
                     :?data (second ?data)
                     :*chain-origin *chain-origin
                     :*chain-local *chain-local})))

(defn make-chsk! [path opts]
  (let [opts (assoc opts :packer (edn-packer {:readers b/data-readers}))]
    (sente/make-channel-socket-client! path opts)))

;; TODO: needs a less brittle implementation...
(defn make-pair-client! [path opts]
  (let [*chain-origin (atom (b/create-chain :gen))
        *chain-local (atom (b/create-chain :gen))
        *freeze-local? (atom false)

        ;; for testing network errors
        *disconnect-> (atom false)
        *disconnect<- (atom false)

        {:keys [chsk ch-recv send-fn state]} (make-chsk! path opts)
        chsk-send! (fn [data reply-fn] (send-fn data 1000 reply-fn))
        injected-handler (comp -handle-msg!
                               #(assoc % :*chain-origin *chain-origin)
                               #(assoc % :*chain-local *chain-local)
                               #(assoc % :*disconnect<- *disconnect<-))]

    ;; pull blocks whenever socket opens
    (add-watch state :state-watch
               (fn [_ _ _ state]
                 (when (:open? state)
                   (chsk-send!
                    [:client/pull-blocks {:base (b/head @*chain-origin)}]
                    (fn [{:as reply :keys [head blocks]}]
                      (println (str "socket opened, got " (count blocks) " blocks!"))
                      (swap! *chain-origin b/link blocks)
                      (swap! *chain-local b/rebase head))))))

    ;; watch local chain
    (b/listen! *chain-local
               (fn [chain-local _]
                 (when (and (:open? @state)
                            (not (empty? chain-local))
                            (not @*freeze-local?)
                            (not @*disconnect->))
                   (swap! *freeze-local? not)
                   (chsk-send!
                    [:client/push-blocks {:blocks chain-local}]
                    (fn [{:as reply :keys [head forward-blocks rebased-blocks]}]
                      (swap! *freeze-local? not)
                      (if (or (not (sente/cb-success? reply))
                              @*disconnect<-)
                        (println "push failed!")
                        (if (= head (b/head chain-local))
                          (do
                            (swap! *chain-origin b/link chain-local)
                            (swap! *chain-local b/chain-since head))
                          (do
                            (println "push succeeded with rebase.")
                            (println "origin:" @*chain-origin)
                            (println "local:" @*chain-local)
                            (println "forward:" forward-blocks)
                            (println "rebased:" rebased-blocks)
                            (swap! *chain-origin b/link forward-blocks rebased-blocks)
                            (swap! *chain-local
                                   (fn [chain-local]
                                     (-> chain-local
                                         (b/chain-since (b/head chain-local))
                                         (b/rebase head))))))))))))

    ;; listen for pushes fr morigin
    (sente/start-client-chsk-router! ch-recv injected-handler)

    [*chain-origin *chain-local *disconnect<- *disconnect->]))
