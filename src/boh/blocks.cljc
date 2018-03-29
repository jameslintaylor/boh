(ns boh.blocks
  (:require [boh.util :refer [sha1]]
            [clojure.spec.alpha :as s]))

;; sha-1 hashes are 40 hexadecimal symbols.
(s/def ::hash #(re-matches #"^:[a-f0-9]{40}$" (str %)))
(s/def ::data string?)
(s/def ::prev (s/nilable ::hash))
(s/def ::block (s/keys :req-un [::prev ::data ::hash]))

(def data-readers {})

(defrecord Block [prev data hash])

(defn content-str
  "Represent the contents of a block as a string. These are the bytes
  from which the hash of the block should be calculated."
  ([{:keys [prev data]}] (content-str prev data))
  ([prev data] (str prev "\n" data)))

(defn hash-block [prev data]
  {:pre [(s/valid? ::prev prev)
         (s/valid? ::data data)]
   :post [(s/valid? ::block %)]}
  (let [hash (sha1 (content-str prev data))]
    (Block. prev data (keyword hash))))

(defn update-meta [obj k update-fn & args]
  (let [m (meta obj)]
    (with-meta obj (apply update m k update-fn args))))

(defn chain-data [base & data]
  {:pre [(s/valid? ::prev base)
         (s/valid? (s/coll-of ::data) data)]}
  (loop [chain []
         prev-block base
         [h & t] data]
    (if (nil? h)
      chain
      (let [b (hash-block prev-block h)]
        (recur (conj chain b) (:hash b) t)))))

(defn chain-data
  "Builds a chain of the data with the given base."
  [base & data]
  {:pre [(s/valid? (s/nilable keyword?) base)]}
  (loop [prev-block base
         [h & t] data
         chain (with-meta [] {:base base
                              :index {}})]
    (if (nil? h)
      chain
      (let [index (count chain)
            block (hash-block prev-block h)]
        (recur (:hash block) t (-> chain
                                   (update-meta :index assoc (:hash block) index)
                                   (conj block)))))))

(defn gen-chain
  "Builds a chain starting with the genesis block."
  [& data]
  (apply chain-data nil data))

(defn chain?
  "Checks if a sequence of blocks forms a contiguous chain. Notably this
  does not check the actual hash for each block."
  [blocks]
  (->> (map vector blocks (drop 1 blocks))
       (every? (fn [[x y]] (= (:prev y) (:hash x))))))

(defn tip [chain]
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
  (apply chain-data base (map :data chain)))

(defn link [& chains]
  (let [[x y & t] chains]
    (if (nil? y)
      x
      (recur (cons (into x y) t)))))

(defn link-data
  [chain & data]
  (let [chain-data (apply chain-data (tip chain) data)]
    (link chain chain-data)))

(defn adjacent? [x y]
  (= (tip x) (base y)))

(defn descendant?
  "Returns true if `y` is a descendant of `x`. A chain is not considered
  to be a descendant of itself."
  [x y]
  (not (empty? (chain-since y (tip x)))))

(defn ends=
  "Cheap way to compare if two chains are 'equal' this doesn't not
  exhaustively check the intermediate hashes for validity."
  [x y]
  (and (= (base x) (base y))
       (= (tip x) (tip y))))

(defn branch-point
  [x y]
  (if (or (ends= x y)
          (descendant? x y))
    (tip x)
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

#_(defrecord ChainRef [*chain]

  p/ChainProxy
  (pull-result [this base]
    (let [chain @*chain]
      {:tip (tip chain)
       :chain (chain-since chain base)}))

  (push-result [this new-chain]
    (let [chain @*chain]
      (if (adjacent? chain new-chain)
        {:tip (tip (swap! *chain link new-chain))}
        (let [missing-chain (chain-since chain (base new-chain))
              rebased-chain (rebase new-chain (tip missing-chain))]
          {:tip (tip (swap! *chain link rebased-chain))
           :?forward-chain (link missing-chain rebased-chain)})))))

#_(defn chain-ref [*chain]
  (->ChainRef *chain))
