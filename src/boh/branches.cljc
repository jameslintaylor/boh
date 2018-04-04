(ns boh.branches
  "helper functions for working on branches/heads."
  (:require [boh.repository :as r]))

(defn descendant?
  "Return true iff the branches given are in `descending` order. That is
  to say that for each branch, it a descendant of the branch before."
  [repo b1 b2]
  (let [b1-head (get-in repo [:heads b1])]
    (some (comp (partial = b1-head) :hash) (r/traverse repo b2))))

(defn descendants?
  "Return true iff the branches given are in `descending` order. That is
  to say that for each branch, it a descendant of the branch before."
  [repo & branches]
  (let [[b & rest] branches]
    (if (empty? rest)
      true
      (if-not (descendant? repo b (first rest))
        false
        (apply descendants? repo rest)))))
