(ns blox-machina.repository-reference
  (:require [blox-machina.blocks :as b]
            [blox-machina.repository :as r]
            [clojure.spec.alpha :as s]))

(s/def ::branch-commits (s/map-of ::branch ::b/chain))
(s/def ::branch-send-failure #{::dangling-base})
(s/def ::branch-send-result (s/or :succeeded ::b/hash
                                  :failed ::branch-push-failure))
(s/def ::branch-recv-result (s/coll-of ::b/block))

(defprotocol RepositoryReference
  (branches! [ref])
  (send-branch! [ref branch blocks])
  (recv-branch! [ref branch hash]))

(defn repo-ref? [x]
  (satisfies? RepositoryReference x))

(defn make-ref! [repo]
  {:pre [(s/valid? ::repo repo)]}
  (let [*repo (atom repo)]
    (reify RepositoryReference
      (branches! [ref]
        (let [repo @*repo]
          (:branches repo)))
      (send-branch! [ref branch blocks]
        (try
          (r/resolve-ref (swap! *repo r/push-branch branch blocks) branch)
          (catch AssertionError e
            ::dangling-base)))
      (recv-branch! [ref branch hash]
        (let [repo @*repo]
          (r/chain repo (r/resolve-ref repo branch) hash))))))
