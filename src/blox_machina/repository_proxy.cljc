(ns blox-machina.repository-proxy
  (:require [blox-machina.blocks :as b]
            [blox-machina.repository :as r]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.core.async :as a :refer [go]]
               :cljs [cljs.core.async :as a])
            [blox-machina.util :include-macros true :refer [do-with]])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defprotocol RepositoryProxy
  (pull! [proxy branches])
  (push! [proxy diff])
  (subscribe! [proxy]))

(s/fdef pull!
        :args (s/cat :ref ::r/ref :branches ::r/branches)
        :ret ::r/repo)

(defn as-proxy [*repo]
  (reify RepositoryProxy
    (pull! [_ version]
      (do-with [ch (a/chan)]
        (a/put! ch (r/diff @*repo version))))
    (push! [_ diff]
      (do-with [ch (a/chan)]
        (a/put! ch (swap! *repo r/merge diff))))
    (subscribe! [_] (println "doesn't support subscribe!"))))
