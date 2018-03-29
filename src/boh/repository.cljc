(ns boh.repository
  (:refer-clojure :exclude [merge bases])
  (:require [boh.blocks :as b]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::branch (comp #{"-" "*"} namespace))
(s/def ::ref (s/or :hash (s/nilable ::b/hash) :branch ::branch))
(s/def ::heads (s/map-of ::branch (s/nilable ::b/hash)))
(s/def ::blocks (s/map-of ::b/hash ::b/block))
(s/def ::repo (s/keys :req-un [::heads ::blocks]))

(defrecord Repository [heads blocks])
(defrecord RepositoryStep [repo-before diff repo-after])

;; TODO - use an explicit repository difference instead of the current
;; implicit method of reusing a regular repository.
(defrecord RepositoryDiff [new-heads new-blocks
                           removed-heads removed-blocks])

;; TODO - use spec's fdef instead of :pre and :post conditions

(defn empty-repo []
  (->Repository {} {}))

(defn head-diff [heads]
  (->Repository heads {}))

(defn block-diff [blocks]
  (->Repository {} blocks))

(defn identity-step [repo]
  (->RepositoryStep repo (empty-repo) repo))

(defn traverse
  "Returns a lazy sequence of the blocks along one path in a blocks
  database."
  [blocks hash]
  (let [block (blocks hash)]
    (when (some? block)
      (lazy-seq (cons block (traverse blocks (:prev block)))))))

(defn resolve-ref
  "Resolves a ref to the its corresponding hash."
  [repo ref]
  (case (first (s/conform ::ref ref))
    :branch (get-in repo [:heads ref])
    :hash ref))

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

(defn branches [repo]
  (keys (:heads repo)))

(defn base
  "Find the base of a branch by fully traversing its chain. If no 'tip'
  block exists, for a branch (right now only used symbolically as part
  of a diff), then the base is just the head."
  [repo branch]
  (let [hash (get-in repo [:heads branch])]
    (if (nil? (get-in repo [:blocks hash]))
      hash
      (-> (:blocks repo) (traverse hash) last :prev))))

(defn bases
  "Compute the base for each branch in a repo. Note that this causes
  each branch to be traversed in full."
  [repo]
  (into {} (map (juxt identity (partial base repo))) (branches repo)))

(defn adjacent?
  "Return true iff the bases are adjacent to the heads. This is not
  strictly `=` since it should tolerate missing branches."
  [heads bases]
  (every? (fn [[branch base]] (= base (heads branch))) bases))

(defn merge-repos
  "Naively merge two repos, giving precedence to the second."
  [repo-into repo]
  (-> repo-into
      (update :heads into (:heads repo))
      (update :blocks into (:blocks repo))))

(defn step [repo diff]
  (RepositoryStep. repo diff (merge-repos repo diff)))

(defn clean [repo]
  ;; check for nil'ed heads here - which is a bit of a hack but the
  ;; only way to support branch deletion right now
  (update repo :heads (fn [m] (into {} (filter (comp m first)) m))))

(defn clean-step [step]
  ;; really should make a more explicit RepositoryStep primitive.
  (-> step
      (update :diff clean)
      (update :repo-after clean)))

(defn join-step
  "Join a previous step with the result of a new step. Use this to
  gradually build a more complex step by aggregating multiple simpler
  operations on a repository."
  [previous-step step-fn & args]
  (let [{:keys [repo-before repo-after diff]} previous-step
        next-step (apply step-fn repo-after args)]
    (RepositoryStep. repo-before
                     (merge-repos diff (:diff next-step))
                     (:repo-after next-step))))

(defn part-step
  "Partially joined step, returns a function from step -> step. The idea
  is that this will make joining step via something like comp easier
  than writing (partial join-step ...)"
  [step-fn & args]
  (fn [step] (apply join-step step step-fn args)))

(defn add-block
  "Add a single block to the repository."
  [repo block]
  (step repo (block-diff {(:hash block) block})))

(defn tag-block [block]
  [(:hash block) block])

(defn add-blocks
  "Bulk add multiple blocks to the repository."
  [repo blocks]
  (let [blocks (into {} (map tag-block) blocks)]
    (step repo (block-diff blocks))))

(defn upsert-branch
  "Change a branches head. If the branch does not exist, adds the branch
  to the repository."
  [repo branch ref]
  (step repo (head-diff {branch (resolve-ref repo ref)})))

(defn upsert-branches
  "Upsert multiple branches in one step by supplying a heads map."
  [repo heads]
  (step repo (head-diff (reduce (fn [m [b r]] (assoc m b (resolve-ref repo r))) {} heads))))

(defn delete-branch
  "Remove the branch from the repo. This does not remove any blocks."
  [repo branch]
  (step repo (head-diff {branch nil})))

(defn graft-block
  "'Move' a branches head forward by a single block. The block must be
  consecutive to the current branch."
  [repo branch block]
  (-> repo
      (add-block block)
      (join-step upsert-branch branch (:hash block))))

(defn graft-chain
  "Move a branches head forward by a chain of blocks. The blocks must
  form a chain and be consecutive to the current branch."
  [repo branch chain]
  (-> repo
      (add-blocks chain)
      (join-step upsert-branch branch (b/tip chain))))

(defn commit
  "Move a branches head forward by computing a block for the data and
  then grafting it to the branch."
  [repo branch data]
  (let [hash (resolve-ref repo branch)
        block (b/hash-block hash data)]
    (graft-block repo branch block)))

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
  "Rebase one branch onto another. This does not remove any of the old
  blocks in the repository."
  [repo branch-onto branch]
  (let [branch-point (branch-point repo branch-onto branch)
        blocks (chain repo branch branch-point)
        rebased-blocks (b/rebase blocks (resolve-ref repo branch-onto))]
    (-> (add-blocks repo rebased-blocks)
        (join-step upsert-branch branch (b/tip rebased-blocks)))))

(defn rebase-merge-branch
  "Rebase a branch onto another, and merges. This removes the branch
  from the repository."
  [repo branch-into branch]
  (-> (rebase-branch repo branch-into branch)
      (join-step upsert-branch branch-into branch)
      (join-step delete-branch branch)))

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
  "Compute a naive diff since a previous version of the repo."
  [repo prev-version]
  (let [heads (:heads repo)
        version (hydrate-version prev-version heads)]
    (Repository. heads (new-blocks repo version))))

(defn narrow
  "Narrow a repository to only contain the branches specified."
  [repo & branches]
  (update repo :heads select-keys branches))

(defn prune
  "Removes all blocks not associated with a branch."
  [repo]
  (let [{:keys [heads blocks]} repo
        tips (map second heads)
        chains (map (partial chain repo) tips)
        blocks (into {} (map tag-block (flatten chains)))]
    (Repository. (:heads repo) blocks)))

(defn contains-version?
  "Naive check to see if a repo contains a version of heads."
  [repo version]
  (every? (:blocks repo) (filter some? (vals version))))
