(ns blox-machina.verification
  (:require [blox-machina.util :refer [concat-sha1]]))

(defn chain?
  "Checks that hash equals prev-block for all consecutive blocks. Of
  note this doesn't recalculate the hashes to verify that blocks
  haven't been tampered with."
  [blocks]
  (let [[x y & t] blocks]
    (if (nil? y)
      true
      (if (not= (:hash x) (:prev-block y))
        false
        (recur t)))))

(defn untampered-block? [block]
  (= (:hash block)
     (concat-sha1 (:prev-block block) (:data block))))

(defn untampered-chain? [blocks]
  (and (chain? blocks)
       (every? untampered-block? blocks)))
