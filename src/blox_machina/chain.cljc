(ns blox-machina.chain
  (:require [blox-machina.blocks :as b]))

(defprotocol Chain
  (-base [this])
  (-head [this])
  (-cons [this block] "Build the chain by adding the blocks onto the end.")
  (-drop [this base] "Return the chain obtained by dropping all blocks up to and including `base`."))

(def chain? (partial instance? Chain))

(defn link [& chains]
  {:pre (every? chain? chains)}
  (loop [[x y & t] chains]
    (into)))

(defn rebase [chain new-base]
  ())

(deftype VectorChain [blocks index base]

  clojure.lang.IPersistentCollection
  (count [] )
  (cons [^Object arg])
  (empty [])
  (equiv [^Object arg])
  
  clojure.lang.Seqable
  (seq [this]
    (seq blocks))

  Chain
  (-base [this]
    (if (empty? blocks)
      base
      (:prev-block (first blocks))))

  (-head [this]
    (:hash (last blocks)))

  (-cons [this block]
    {:pre (= (:prev-block block) (-head this))}
    (VectorChain. (conj blocks block)
                  (assoc index (:hash block) (count blocks))
                  base))

  (-drop [this hash]
    {:pre (or (= hash base) (contains? index base))}
    (if-let [offset (hash index)]
      (let [[dropped new-blocks] (split-at (inc offset) blocks)]
        (VectorChain. new-blocks
                      (dissoc (apply dissoc index (map :hash dropped)))
                      hash))
      (if (= hash base)
        this
        nil))))

(defn ^VectorChain vchain [base & data]
  (loop [prev-block base
         blocks []
         offset 0
         index {}
         [h & t] data]
    (if (nil? h)
      (VectorChain. blocks index base)
      (let [block (b/create-block prev-block h)
            id (:hash block)]
        (recur id
               (conj blocks block)
               (inc offset)
               (assoc index id offset)
               t)))))
