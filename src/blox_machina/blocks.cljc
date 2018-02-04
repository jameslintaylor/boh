(ns blox-machina.blocks
  (:require [blox-machina.util :refer [concat-sha1]]))

(defrecord Block [prev-block data hash])

(def data-readers {'blox-machina.blocks.Block map->Block
                   'blox_machina.blocks.Block map->Block})

(defn next-block [prev-block data]
  (let [hash (concat-sha1 prev-block data)]
    (Block. prev-block data hash)))

(defn update-meta [obj k update-fn & args]
  (let [m (meta obj)]
    (with-meta obj (apply update m k update-fn args))))

(defn create-chain
  "Builds a chain of the data with the given base."
  [base & data]
  {:pre (keyword? base)}
  (loop [prev-block base
         [h & t] data
         chain (with-meta [] {:base base
                              :index {}})]
    (if (nil? h)
      chain
      (let [index (count chain)
            block (next-block prev-block h)]
        (recur (:hash block) t (-> chain
                                   (update-meta :index assoc (:hash block) index)
                                   (conj block)))))))

(defn chain? [blocks]
  (->> (map vector blocks (drop 1 blocks))
       (every? (fn [[x y]] (= (:prev-block y) (:hash x))))))

(defn head [chain]
  (if (empty? chain)
    (:base (meta chain))
    (:hash (last chain))))

(defn base [chain]
  (if (empty? chain)
    (:base (meta chain))
    (:prev-block (first chain))))

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
  (let [chain-data (apply create-chain (head chain) data)]
    (link chain chain-data)))

(defn adjacent? [x y]
  (= (head x) (base y)))

(defn descendant?
  "Returns true if `y` is a descendant of `x`. A chain is not considered
  to be a descendant of itself."
  [x y]
  (not (empty? (chain-since y (head x)))))

(defn ends=
  "Cheap way to compare if two chains are 'equal' this doesn't not
  exhaustively check the intermediate hashes for vaidity."
  [x y]
  (and (= (base x) (base y))
       (= (head x) (head y))))

(defn branch-point
  [x y]
  (if (or (ends= x y)
          (descendant? x y))
    (head x)
    (if-let [[a _] (->> (map vector x y)
                        (filter (fn [[a b]] (not= (:hash a) (:hash b))))
                        first)]
      (:prev-block a)
      (base x))))

(defn diff
  [chain-from chain-to]
  (let [branch-point (branch-point chain-from chain-to)]
    {:- (chain-since chain-from branch-point)
     :+ (chain-since chain-to branch-point)}))

(defn ^{:style/indent 1} listen!
  "Listen for changes to a chain ref. The callback-fn should be a
  2-arity function taking the value of the entire chain aswell as the
  new blocks (diff). Note that if the ancestry changes, the delta is
  empty. Returns a 0-arity function that unregisters the listener."
  [*chain callback-fn]
  (let [key (rand-int 1000)]
    (add-watch *chain key
               (fn [_ _ old new]
                 (callback-fn new (diff old new))))
    (fn [] (remove-watch *chain key))))
