(ns boh.projection
  (:require [boh.repository :as r]
            [boh.repository-reference :as rr]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [boh.util :refer [do-with]])))

(defn projection [ref branch reducing-fn]
  (let [*v (atom (reducing-fn) :meta {:tag nil})
        *cache (volatile! {})
        ch (rr/listen! ref :step :upstream-step)]
    (go-loop []
      (let [{:keys [repo-before diff repo-after]} (a/<! ch)]
        (when-some [new-head (get-in diff [:heads branch])]
          (let [cache @*cache
                blocks (-> (into []
                                 (take-while (comp (complement cache) :hash))
                                 (r/traverse repo-after new-head))
                           (reverse))
                cached (get cache (:prev (first blocks)))]
            (when (some? blocks)
              (vswap! *cache assoc new-head
                      (reset! *v (transduce (map :data) reducing-fn
                                            (or cached (reducing-fn)) blocks))))))
        (recur)))
    *v))
