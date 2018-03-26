(ns blox-machina.repository
  (:refer-clojure :exclude [merge])
  (:require [blox-machina.blocks :as b]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::branch #(re-matches #"^:-[/\-a-z0-9]+" (str %)))
(s/def ::ref (s/or :hash (s/nilable ::b/hash) :branch ::branch))
(s/def ::branches (s/map-of ::branch (s/nilable ::b/hash)))
(s/def ::blocks (s/map-of ::b/hash ::b/block))
(s/def ::repo (s/keys :req-un [::branches ::blocks]))

;; TODO - use spec's fdef instead of :pre and :post conditions

(defrecord Repository [branches blocks])
(defrecord RepositoryStep [repo-before diff repo-after])

(defn empty-repo []
  (Repository. {} {}))

(defn branch-diff [branches]
  (Repository. branches {}))

(defn block-diff [blocks]
  (Repository. {} blocks))

(defn resolve-ref
  "Resolves a ref to the its corresponding hash."
  [repo ref]
  (case (first (s/conform ::ref ref))
    :branch (get-in repo [:branches ref])
    :hash ref))

(defn merge
  "Naively merge two repos, giving precedence to the second."
  [repo-into repo]
  (-> repo-into
      (update :branches into (:branches repo))
      (update :blocks into (:blocks repo))))

(defn step [repo diff]
  (RepositoryStep. repo diff (merge repo diff)))

(defn add-block
  "Add a single block to the repository."
  [repo block]
  (step repo (block-diff {(:hash block) block})))

(defn add-blocks
  "Bulk add multiple blocks to the repository."
  [repo blocks]
  (step repo (block-diff (into {} (map (fn [b] [(:hash b) b]) blocks)))))

(defn upsert-branch
  "Change the hash that a branch points to. If the branch does not exist,
  adds the branch to the repository."
  [repo branch hash]
  (step repo (branch-diff {branch hash})))

(defn delete-branch
  "Remove the branch from the repo. This does not remove any blocks."
  [repo branch]
  (step repo (branch-diff {branch nil})))

(defn tag-block [block]
  [(:hash block) block])

(defn graft-block
  "Move a branch forward by moving it forward a single block. The block
  must be consecutive to the current branch."
  [repo branch block]
  (-> repo
      (add-block block) :repo-after
      (upsert-branch branch (:hash block))))

(defn graft-chain
  "Move a branch forward by moving it forward multiple blocks.
  The blocks must be consecutive to the current branch."
  [repo branch chain]
  (-> repo
      (add-blocks chain) :repo-after
      (upsert-branch branch (b/tip chain))))

(defn commit
  "Convenience function. Move a branch forward by computing a block for
  the data and pointing the branch to that block."
  [repo branch data]
  (let [current-hash (resolve-ref repo branch)
        block (b/hash-block current-hash data)]
    (graft-block repo branch block)))

(defn traverse
  "Returns a lazy sequence of the blocks along one path in a blocks
  database."
  [blocks hash]
  (let [block (blocks hash)]
    (when (some? block)
      (lazy-seq (cons block (traverse blocks (:prev block)))))))

(defn chain
  "Retrieve the chain whom's tip is at ref-tip and base is at ref-base.
  O(n)."
  ([repo ref-tip] (chain repo ref-tip nil))
  ([repo ref-tip ref-base]
   (let [hash-tip (resolve-ref repo ref-tip)
         hash-base (resolve-ref repo ref-base)]
     (->> (traverse (:blocks repo) hash-tip)
          (take-while (comp (partial not= hash-base) :hash))
          reverse))))

(defn branch-point
  "Find the branch-point (the greatest common ancestor) between two
  branches in a repository."
  [repo ref1 ref2]
  (let [block-database (:blocks repo)
        hash1 (resolve-ref repo ref1)
        hash2 (resolve-ref repo ref2)
        blocks1 (map :hash (traverse block-database hash1))
        blocks2 (map :hash (traverse block-database hash2))
        members1 (apply hash-set blocks1)]
    (some members1 blocks2)))

(defn rebase-branch
  "Rebase a branch onto another. This does not remove any of the old
  blocks in the repository."
  [repo branch-onto branch]
  (let [anc (branch-point repo branch-onto branch)
        blocks (chain repo branch anc)
        rebased-blocks (b/rebase blocks (resolve-ref repo branch-onto))]
    (-> repo
        (add-blocks rebased-blocks) :repo-after
        (upsert-branch branch (b/tip rebased-blocks)))))

(defn merge-branch
  "Merge a branch into another. This removes the branch from the
  repository."
  [repo branch-into branch]
  (let [anc (branch-point repo branch-into branch)
        blocks (chain repo branch anc)
        ;; all merging just follows a rebase technique right now
        rebased-blocks (b/rebase blocks (resolve-ref repo branch-into))]
    (-> repo
        (add-blocks rebased-blocks) :repo-after
        (upsert-branch branch-into (b/tip rebased-blocks)) :repo-after
        (delete-branch branch))))

(defn with-intersection
  "Update map a with the intersection from map b."
  [a b]
  (reduce-kv
   (fn [m k v]
     (assoc m k (or (b k) v)))
   {} a))

(defn hydrate-version
  "'Hydrate' a potentially incomplete specification of a previous
  version with the missing branches from a reference version."
  [prev-version ref-version]
  (-> (into {} (map vector (keys ref-version) (repeat nil)))
      (with-intersection prev-version)))

(defn new-blocks
  [repo version]
  (let [tf (comp (map (partial apply chain repo))
                 (map (partial map tag-block)))
        tagged-blocks (apply concat (into [] tf version))]
    (into {} tagged-blocks)))

(defn diff
  "Return the diff since a previous version of the repo."
  [repo prev-version]
  (let [branches (:branches repo)
        version (hydrate-version prev-version branches)]
    (Repository. branches
                 (new-blocks repo version))))

(defn prune
  "Removes all blocks not associated with a branch."
  [repo]
  (let [{:keys [branches blocks]} repo
        tips (map second branches)
        chains (map (partial chain repo) tips)
        blocks (into {} (map tag-block (flatten chains)))]
    (Repository. (:branches repo)
                 blocks)))
