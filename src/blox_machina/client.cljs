(ns blox-machina.client
  (:require [blox-machina.util :refer [edn-packer callback-chan! pipe]]
            [cljs.core.async :as a]
            [taoensso.sente :as sente]
            [blox-machina.blocks :as b])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn- tag [id]
  (map (fn [x] [id x])))

(defn- response-chan! [chsk-send! msg]
  (callback-chan! (partial chsk-send! msg 1000)))

(defn- watch-chan! [iref]
  (callback-chan! (partial add-watch iref (rand))
                  (fn [_ _ _ new]
                    new)))

(defn pull-msg [base] [:client/pull {:base base}])
(defn push-msg [blocks] [:client/push {:blocks blocks}])

(defmulti handle-msg! first)

(defmethod handle-msg! :default
  [[id]]
  (println "client unhandled message" id))

(defmethod handle-msg! :origin/push
  [[_ {:keys [blocks]} push-ch]]
  (a/put! push-ch blocks))

(defmulti handle-event! :id)

(defmethod handle-event! :default
  [{:keys [id]}]
  (println "client unhandled event" id))

(defmethod handle-event! :chsk/ws-ping
  [_])

(defmethod handle-event! :chsk/recv
  [{:keys [push-ch event]}]
  (handle-msg! (conj (second event) push-ch)))

(defn origin-push-chan!
  [ch-recv]
  (let [push-ch (a/chan)
        injected-handler (comp handle-event!
                               #(assoc % :push-ch push-ch))]
    (sente/start-client-chsk-router! ch-recv injected-handler)
    push-ch))

{:state {:blocks-local []
         :blocks-origin []}
 :effects [{:id :sync-server}]}

;; -reduce-push :: state -> push -> (side effects) effect-map
(defmulti -reduce-push (fn [_ push] (:source push)))

(defmethod -reduce-push :origin
  [{:keys [blocks]} state]
  (println "push from origin!")
  {:state state})

(defmethod -reduce-push :local
  [{:keys [blocks]} state]
  (let [[id data] (a/<! r-chan! (push-msg [blocks]))]
    ())
  (println "push from local!")
  state)

;; apply-effect :: chain -> effect-map -> new-chain
(defn apply-effect [chain effect-map]
  chain)

;; handle-push! :: chain-ref -> state -> push -> (side effects) new-state
(defn handle-push! [*chain state r-chan! push]
  (let [effect-map (-reduce-push! state push)]
    (swap! *chain apply-effect effect-map)
    (:state effect-map)))

(defn start-push-loop!
  [*chain ch-recv r-chan!]

  (let [origin-push-ch (pipe (origin-push-chan! ch-recv)
                             (tag :origin))
        local-push-ch (pipe (watch-chan! *chain)
                            (comp (map last)
                                  (map (fn [x] [x]))
                                  (tag :local)))]

    (go-loop [state {:blocks-origin @*chain
                     :blocks-local []}]
      (let [[[source blocks]] (a/alts! [origin-push-ch local-push-ch])
            new-state (handle-push! *chain r-chan! state {:source source :blocks blocks})]
        (recur new-state)))))

(defn chain-with-origin!
  ([path]
   (chain-with-origin! path {}))
  ([path opts]
   (let [chsk (make-chsk! path opts)
         r-chan! (partial response-chan! (:send-fn chsk))
         *chain (atom [])
         state-ch (watch-chan! (:state chsk))]

     (go-loop []
       (let [state (a/<! state-ch)]
         ;; initial pull
         (when (:first-open? state)
           (let [blocks-origin (a/<! (r-chan! (pull-msg :gen)))]
             (reset! *chain blocks-origin)
             (start-push-loop! *chain (:ch-recv chsk) r-chan!)))
         (recur)))

     *chain)))

(defn make-chsk! [path opts]
  (let [opts (assoc opts :packer (edn-packer {:readers b/data-readers}))]
    (sente/make-channel-socket-client! path opts)))

(defn make-buffered-send-fn [send-fn]
  (let [*busy (atom false)
        *queue (atom ())

        wrapped-send (fn [data reply-fn]
                       (reset! *busy true)
                       (send-fn data 1000
                                (fn [reply]
                                  (println "got reply!")
                                  (if (sente/cb-success? reply)
                                    (do
                                      (reply-fn reply)
                                      (reset! *busy false))
                                    (do
                                      (println "network disconnect?")
                                      (swap! *queue empty)
                                      (reset! *busy false))))))

        buffered-send (fn [data reply-fn]
                        (println "invoked" data)
                        (if @*busy
                          (do
                            (println "buffer is busy, queueing send.")
                            (swap! *queue conj [data reply-fn]))
                          (wrapped-send data reply-fn)))]

    (add-watch *busy (rand-int 1000)
               (fn [_ _ _ busy]
                 (println "busy:" busy)
                 (when-not (or busy (empty? @*queue))
                   (println "sending queued")
                   (apply buffered-send (first @*queue))
                   (swap! *queue rest))))

    buffered-send))

(defn make-pair-client! [path opts]
  (let [*chain-origin (atom (b/create-chain :gen))
        *chain-local (atom (b/create-chain :gen))
        {:keys [chsk ch-recv send-fn state]} (make-chsk! path opts)
        buffered-send! (fn [data reply-fn] (send-fn data 1000 reply-fn)) #_(make-buffered-send-fn send-fn)]

    ;; pull blocks whenever socket opens
    (add-watch state :state-watch
               (fn [_ _ _ state]
                 (when (:open? state)
                   (buffered-send!
                    [:client/pull-blocks {:base (b/head @*chain-origin)}]
                    (fn [{:keys [head blocks]}]
                      (print (str "socket opened, got " (count blocks) " blocks!"))
                      (swap! *chain-origin b/link blocks)
                      (swap! *chain-local b/rebase head))))))

    ;; watch local chain
    (b/listen! *chain-local
               (fn [chain new-blocks]
                 (when (and (:open? @state)
                            (not (empty? new-blocks)))
                   (println "pushing" new-blocks)
                   (buffered-send!
                    [:client/push-blocks {:blocks new-blocks}]
                    (fn [{:keys [head rebased-blocks]}]
                      (if (= head (b/head new-blocks))
                        (do
                          (println "push succeeded!")
                          (swap! *chain-origin b/link new-blocks)
                          (swap! *chain-local
                                 (fn [chain-local]
                                   (-> chain-local
                                       (b/chain-since head)
                                       (with-meta {:base head})))))
                        (do
                          (println "push succeeded with rebase.")
                          (swap! *chain-origin b/link rebased-blocks)
                          (swap! *chain-local
                                 (fn [chain-local]
                                   (-> chain-local
                                       (b/chain-since (b/head new-blocks))
                                       (b/rebase head)))))))))))

    [*chain-origin *chain-local]))
