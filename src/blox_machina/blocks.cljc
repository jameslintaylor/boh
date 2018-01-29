(ns blox-machina.blocks
  (:require [blox-machina.util :refer [concat-sha1]]
            [blox-machina.verification :as v]))

(defrecord Block [prev-block data hash])

(def data-readers {'blox-machina.blocks.Block map->Block
                   'blox_machina.blocks.Block map->Block})

(defn create-block [prev-block data]
  (let [hash (concat-sha1 prev-block data)]
    (Block. prev-block data hash)))

(defn create-chain
  "Builds a chain of the data with the given base."
  [base & data]
  (loop [prev-block base
         [h & t] data
         chain (with-meta [] {:base base})]
    (if (nil? h)
      chain
      (let [block (create-block prev-block h)]
        (recur (:hash block) t (conj chain block))))))

(defn head [chain]
  (if (empty? chain)
    (:base (meta chain))
    (:hash (last chain))))

(defn base [chain]
  (if (empty? chain)
    (:base (meta chain))
    (:prev-block (first chain))))

(defn consecutive?
  "Returns true if the given chains can be linked together."
  [& chains]
  (let [[x y & t] chains]
    (if (nil? y)
      true
      (if (not= (head x) (base y))
        false
        (recur (cons y t))))))

(defn ancestor?
  "Returns true if the given chains share a common history. The chains
  are expected to be ordered oldest to newest. Note that a chain is
  not considered an ancestor of itself."
  [& chains]
  (let [[x y & t] chains]
    (if (nil? y)
      true
      (let [branch-point (head x)]
        (if-not (some #(= branch-point (:prev-block %)) y)
          false
          (recur (cons y t)))))))

(defn chain-since [chain base]
  (drop-while #(not= base (:prev-block %)) chain))

(defn rebase [chain base]
  (apply create-chain base (map :data chain)))

(defn link [chain blocks]
  {:pre [(v/chain? blocks)
         (= (head chain) (base blocks))]}
  (into chain blocks))

(defn link-data
  [chain & data]
  (let [blocks (apply chain-data (head chain) data)]
    (link chain blocks)))

(defn delta [chain-from chain-to]
  (chain-since chain-to (head chain-from)))

(defn listen!
  [*chain callback-fn]
  (add-watch *chain (rand-int 1000)
             (fn [_ _ old new]
               (if (= old new)
                 (println "metadata changed")
                 (if-not (ancestor? old new)
                   (println "ancestry changed")
                   (callback-fn new (diff old new)))))))
