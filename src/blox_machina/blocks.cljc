(ns blox-machina.blocks
  (:require [blox-machina.util :refer [concat-sha1]]
            [blox-machina.verification :as v]))

(defrecord Block [prev-block data hash])

(def data-readers {'blox-machina.blocks.Block map->Block
                   'blox_machina.blocks.Block map->Block})

(defn create-block [prev-block content]
  (let [hash (concat-sha1 prev-block content)]
    (Block. prev-block content hash)))

(defn head [chain] (:hash (last chain)))
(defn base [chain] (:prev-block (first chain)))

(defn consecutive?
  "Returns true if the given chains can be linked together."
  [& chains]
  (let [[x y & t] chains]
    (if (nil? y)
      true
      (if (not= (head x) (base y))
        false
        (recur t)))))

(defn chain-since
  [chain base]
  (drop-while #(not= base (:prev-block %)) chain))

(defn chain-data
  "Builds a chain of the data with the given base."
  [base & data]
  (loop [prev-block base
         [h & t] data
         chain []]
    (if (nil? h) chain
        (let [block (create-block prev-block h)]
          (recur (:hash block) t (conj chain block))))))

(defn rebase [chain base]
  (apply chain-data base (map #(.-data %) chain)))

(defn link [chain & blocks]
  {:pre [(v/chain? blocks)
         (= (head chain) (base blocks))]}
  (into chain blocks))

(defn link-data
  [chain & data]
  (let [blocks (apply chain-data (head chain) data)]
    (apply link chain blocks)))
