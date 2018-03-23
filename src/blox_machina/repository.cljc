(ns blox-machina.repository
  (:require [blox-machina.blocks :as b]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::branch #(re-matches #"^:-[a-z0-9]+$" (str %)))
(s/def ::ref (s/or :hash (s/nilable ::b/hash) :branch ::branch))
(s/def ::branches (s/map-of ::branch (s/nilable ::b/hash)))
(s/def ::blocks (s/map-of ::b/hash ::b/block))
(s/def ::repo (s/keys :req-un [::branches ::blocks]))

(defrecord Repository [branches blocks])

;; TODO - use spec's fdef instead of :pre and :post conditions

(defn empty-repo []
  {:post [(s/valid? ::repo %)]}
  (Repository. {} {}))

(defn resolve-ref
  "Resolves a ref to the its corresponding hash."
  [repo ref]
  {:pre [(s/valid? ::repo repo)
         (s/valid? ::ref ref)]
   :post [(s/valid? (s/nilable ::b/hash) %)]}
  (case (first (s/conform ::ref ref))
    :branch (get-in repo [:branches ref])
    :hash ref))

(defn add-block
  "Add a single block to the repository."
  [repo block]
  {:pre [(s/valid? ::repo repo)
         (s/valid? ::b/block block)]
   :post [(s/valid? ::repo %)]}
  (assoc-in repo [:blocks (:hash block)] block))

(defn add-blocks
  "Bulk add multiple blocks to the repository."
  [repo blocks]
  {:pre [(s/valid? ::repo repo)
         (s/valid? (s/coll-of ::b/block) blocks)]
   :post [(s/valid? ::repo %)]}
  (let [block-map (into {} (map (fn [b] [(:hash b) b]) blocks))]
    (update repo :blocks into block-map)))

(defn upsert-branch
  "Change the hash that a branch points to. If the branch does not exist,
  adds the branch to the repository."
  [repo branch hash]
  {:pre [(s/valid? ::repo repo)
         (s/valid? ::branch branch)
         (s/valid? ::b/hash hash)]
   :post [(s/valid? ::repo %)]}
  (assoc-in repo [:branches branch] hash))

(defn delete-branch
  "Remove the branch from the repo. This does not remove any blocks."
  [repo branch]
  {:pre [(s/valid? ::repo repo)
         (s/valid? ::branch branch)]
   :post [(s/valid? ::repo %)]}
  (update repo :branches dissoc branch))

(defn prune
  "Removes all blocks not associated with a branch."
  [repo]
  {:pre [(s/valid? ::repo repo)]
   :post [(s/valid? ::repo %)]}
  "not implemented yet")

(defn push-block
  "Push a branch forward by moving it forward a single block. The block
  must be consecutive to the current branch."
  [repo branch block]
  {:pre [(s/valid? ::repo repo)
         (s/valid? ::branch branch)
         (s/valid? ::b/block block)
         (= (:prev-block block) (resolve-ref repo branch))]
   :post [(s/valid? ::repo %)]}
  (-> repo
      (add-block block)
      (upsert-branch branch (:hash block))))

(defn push-chain
  "Push a branch forward by moving it forward multiple blocks.
  The blocks must be consecutive to the current branch."
  [repo branch chain]
  {:pre [(b/chain? chain)
         (= (b/base chain) (resolve-ref repo branch))]}
  (-> repo
      (add-blocks chain)
      (upsert-branch branch (b/tip chain))))

(defn push-chains
  "Push multiple branches forward."
  [repo chains]
  {:pre [(s/valid? (s/map-of ::branch b/chain?) chains)]}
  (reduce (fn [r [b c]] (push-chain r b c)) repo chains))

(defn commit
  "Convenience function. Push a branch forward by computing a block for
  the data and pointing the branch to that block."
  [repo branch data]
  (let [current-hash (resolve-ref repo branch)
        block (b/hash-block current-hash data)]
    (push-block repo branch block)))

(defn chain
  "Retrieve the chain whom's tip is at ref-tip and base is at ref-base.
  O(n)."
  ([repo ref-tip] (chain repo ref-tip nil))
  ([repo ref-tip ref-base]
   (let [blocks (:blocks repo)
         hash-tip (resolve-ref repo ref-tip)
         hash-base (resolve-ref repo ref-base)]
     (loop [chain ()
            next-block hash-tip]
       (let [block (get blocks next-block)]
         (if (or (nil? block) (= hash-base (:hash block)))
           chain
           (recur (conj chain block) (:prev-block block))))))))

(defn chains
  [repo branches]
  (into {} (map (fn [[branch [ref-tip ref-base]]] [branch (chain ref-tip ref-base)]) branches)))

(defn repo-with [& data]
  (let [chain (apply b/gen-chain data)]
    (-> (empty-repo)
        (push-chain :-master chain))))
