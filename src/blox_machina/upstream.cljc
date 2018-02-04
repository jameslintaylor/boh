(ns blox-machina.upstream
  (:require [blox-machina.blocks :as b]
            #?(:clj [clojure.core.async :as a]
               :cljs [cljs.core.async :as a])))

(defprotocol Upstream
  (pull [this base])
  (push [this blocks]))

(defprotocol UpstreamAsync
  (pull-chan [this base])
  (push-chan [this chain]))

(defn upstream-async
  "Lifts an instance of Upstream to an instance of UpstreamAsync."
  [u]
  {:pre (instance? Upstream u)}
  (reify UpstreamAsync
    (pull-chan [this base]
      (let [chan (a/chan)]
        (a/put! chan (pull u base))
        chan))
    (push-chan [this chain]
      (let [chan (a/chan)]
        (a/put! chan (push u base))
        chan))))

(defprotocol UpstreamTrack
  (chan-recv! [this]))

(deftype UpstreamBranch [branch]

  Upstream
  (pull [this base]
    (let [blocks (b/chain-since @*chain base)]
      {:head (b/head blocks)
       :blocks blocks}))

  (push! [this blocks]
    (let [chain (swap! *chain b/link blocks)]
      {:head (b/head blocks)}))

  IUpstreamTrack
  (chan-recv! [this]
    (let [chan (a/chan)]
      (b/listen! *chain
        (fn [_ delta]
          (a/put! chan {:head (b/head delta)
                        :blocks delta})))
      chan)))
