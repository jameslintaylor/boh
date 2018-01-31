(ns blox-machina.blocks
  (:require [blox-machina.util :refer [concat-sha1]]
            [blox-machina.verification :as v]))

;; new architecture

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
  (-> (into [] (drop-while #(not= base (:prev-block %)) chain))
      (with-meta {:base base})))

(defn rebase [chain base]
  (apply create-chain base (map :data chain)))

(defn link [& chains]
  (let [[x y & t] chains]
    (if (nil? y)
      x
      (recur (cons (into x y) t)))))

(defn link-data
  [chain & data]
  (let [blocks (apply create-chain (head chain) data)]
    (link chain blocks)))

(defn delta [chain-from chain-to]
  (chain-since chain-to (head chain-from)))

(defn ^{:style/indent 1} listen!
  "Listen for changes to a chain ref. The callback-fn should be a
  2-arity function taking the value of the entire chain aswell as the
  new blocks (delta). Note that if the ancestry changes, the delta is
  empty. Returns a 0-arity function that unregisters the listener."
  [*chain callback-fn]
  (let [key (rand-int 1000)]
    (add-watch *chain key
               (fn [_ _ old new]
                 (callback-fn new (delta old new))))
    (fn [] (remove-watch *chain key))))
