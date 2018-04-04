(ns boh.projection
  (:require [boh.repository :as r]
            [boh.repository-reference :as rr]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a])
            #?(:clj [boh.util :refer [do-with]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [boh.util :refer [do-with]])))

(defn reduce-audit
  ([rf coll] (reduce-audit rf (rf) coll))
  ([rf init coll]
   (loop [c (seq coll)
          a init
          trail (transient [])]
     (if-not c
       (persistent! trail)
       (let [nval (rf a (first c))]
         (if (reduced? nval)
           (persistent! (conj! trail @nval))
           (recur (next c) nval (conj! trail nval))))))))

(defrecord Frame [prev image hash])

(defn frames [cache repo xform data-rf head]
  (let [chain (->> (r/traverse repo head)
                   (take-while (comp (complement cache) :hash))
                   (reverse))
        base (:prev (first chain))
        block-rf ((comp (map :data) xform) data-rf)
        imgs (reduce-audit block-rf (or (get-in cache [base :image])
                                      (block-rf)) chain)]
    (into {} (map (fn [{:keys [hash prev]} img]
                    [hash (->Frame prev img hash)])
                  chain imgs))))

(defn update-frames [fs heads repo xform data-rf]
  (apply merge fs (map #(frames fs repo xform data-rf (second %)) heads)))

(defn projection-xf [xform data-rf]
  (let [*fs (volatile! {})]
    (fn [rf]
      (fn
        ([] (rf))
        ([a] (rf a))
        ([a {:keys [diff repo-after]}]
         (let [fs (vswap! *fs update-frames (:heads diff) repo-after xform data-rf)]
           (rf a {:heads (:heads repo-after)
                  :frames fs})))))))

(defn projection!
  "Return a channel of projections of a ref with the given reducing-fn.
  Optionally pass a transducer to wrap the reducing-fn."
  [ref xform reducing-fn]
  (let [listen-ch (rr/listen! ref :step :upstream-step)
        xf (projection-xf xform reducing-fn)]
    (do-with [ch (a/chan)]
      (a/pipeline 1 ch xf listen-ch))))
