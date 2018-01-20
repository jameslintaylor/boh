(ns blox-machina.client
  (:require [blox-machina.util :as util]
            [cljs.core.async :as a]
            [taoensso.sente :as sente]
            [blox-machina.blocks :as b])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn make-chsk! [path opts]
  (sente/make-channel-socket-client! path (assoc opts :packer util/packer)))

(defn callback-chan!
  "Takes a callback-consuming function and registers with it a callback
  that passes on the values to the returned channel using an optional
  packing function."
  ([register-fn] (callback-chan! register-fn identity))
  ([register-fn f]
   (let [chan (a/chan)]
     (register-fn (fn [& xs]
                    (a/put! chan (apply f xs))))
     chan)))

(defn- pipe [in xf]
  (let [out (a/chan)]
    (a/pipeline 4 out xf in)
    out))

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

;; -reduce-push :: state -> push -> effect-map
(defmulti -reduce-push (fn [_ push] (:source push)))

(defmethod -reduce-push :origin
  [{:keys [blocks]} state]
  (println "push from origin!")
  {:state state})

(defmethod -reduce-push :local
  [{:keys [blocks]} state]
  (println "push from local!")
  state)

;; apply-effect :: chain -> effect-map -> new-chain
(defn apply-effect [chain effect-map]
  chain)

;; handle-push! :: chain-ref -> state -> push -> (side effects) new-state
(defn handle-push! [*chain state push]
  (let [effect-map (-reduce-push state push)]
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
            new-state (handle-push! *chain state {:source source :blocks blocks})]
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
