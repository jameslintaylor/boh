(ns boh.repository
  (:refer-clojure :exclude [merge bases])
  (:require [boh.blocks :as b]
            [boh.util :refer [keys-containing select-containing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::branch (comp #{"-" "~"} namespace))
(s/def ::ref (s/or :hash (s/nilable ::b/hash) :branch ::branch))
(s/def ::heads (s/map-of ::branch (s/nilable ::b/hash)))
(s/def ::blocks (s/map-of ::b/hash ::b/block))
(s/def ::repo (s/keys :req-un [::heads ::blocks]))

(defrecord Repository [heads blocks])
(defrecord RepositoryStep [repo-before diff repo-after])

;; as convenience for testing
(defn -repo-with [& data]
  (let [chain (apply b/gen-chain (map str data))]
    (->Repository {:-/master (b/tip chain)}
                  (into {} (map (juxt :hash identity)) chain))))

(defn empty-repo []
  (->Repository {} {}))

(defn identity-diff []
  (->Repository {} {}))

(defn identity-step [repo]
  (->RepositoryStep repo (identity-diff) repo))

(defn resolve-ref
  "Resolves a ref to its corresponding hash in a repository."
  [repo ref]
  (case (first (s/conform ::ref ref))
    :branch (get-in repo [:heads ref])
    :hash ref))

(defn- -traverse [blocks hash]
  (when-some [block (get blocks hash)]
    (lazy-seq (cons block (-traverse blocks (:prev block))))))

(defn traverse
  "Returns a lazy sequence of the blocks in a repository by following
  the path starting at ref."
  [repo ref]
  (let [hash (resolve-ref repo ref)]
    (-traverse (:blocks repo) hash)))

(defn chain
  "Retrieve the chain whom's tip is at ref-tip and base is at ref-base.
  O(n)."
  ([repo ref-tip] (chain repo ref-tip nil))
  ([repo ref-tip ref-base]
   (let [hash-base (resolve-ref repo ref-base)]
     (->> (traverse repo ref-tip)
          (take-while (comp (partial not= hash-base) :hash))
          reverse))))

(defn merge-repos
  "Naively merge two repos, giving precedence to the second."
  [repo-into repo]
  (-> repo-into
      (update :heads into (:heads repo))
      (update :blocks into (:blocks repo))))

(defn apply-diff
  "Apply a diff to a repository."
  [repo diff]
  (let [{:keys [heads blocks]} diff]
    (-> repo
        (update :heads into (select-containing some? heads))
        (update :blocks into (select-containing some? blocks))
        (update :heads (partial apply dissoc) (keys-containing nil? heads))
        (update :blocks (partial apply dissoc) (keys-containing nil? blocks)))))

(defn version-diff
  "Find the difference between two versions."
  [version prev-version]
  (reduce (fn [m [b h]] (assoc m b (m b))) version prev-version))

(defn block-diff
  "Find the blocks added to the repo since the version."
  [repo prev-version]
  (let [backwards-diff (version-diff prev-version (:heads repo))]
    (into {} (comp (map (partial apply chain repo))
                   (mapcat identity)
                   (map (juxt :hash identity)))
          backwards-diff)))

(defn diff
  "Calculate the diff between a repo and a previous version."
  [repo prev-version]
  (->Repository (version-diff (:heads repo) prev-version)
                (block-diff repo prev-version)))

(defn base
  "Find the base of a branch by fully traversing its chain. If a branch
  points to a head that does not exist in the repo then the base is
  the head."
  [repo branch]
  (let [head (get-in repo [:heads branch])]
    (if (nil? (get-in repo [:blocks head]))
      head
      (-> (traverse repo head) last :prev))))

(defn bases
  "Compute the base for each branch in a repo. This causes each branch
  to be traversed in full."
  [repo]
  (into {} (map (juxt identity (partial base repo))) (keys (:heads repo))))

(defn adjacent?
  "Returns true iff the bases are adjacent to the heads. This is not
  strictly `=` since it should tolerate missing branches."
  [heads bases]
  (every? (fn [[branch base]] (= base (heads branch))) bases))

(defn includes?
  "Returns true iff a repo includes/contains a version. Loosely this
  could be interpreted as a repo being a successor of the version."
  [repo version]
  (every? (:blocks repo) (filter some? (vals version))))

(defn step [repo diff]
  (RepositoryStep. repo diff (apply-diff repo diff)))

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

;; STEP FUNCTIONS

(defn add-block
  "Add a single block to the repository."
  [repo block]
  (step repo (->Repository {} {(:hash block) block})))

(defn add-blocks
  "Bulk add multiple blocks to the repository."
  [repo blocks]
  (let [blocks (into {} (map (juxt :hash identity)) blocks)]
    (step repo (->Repository {} blocks))))

(defn upsert-branch
  "Change a branches head. If the branch does not exist, adds the branch
  to the repository."
  [repo branch ref]
  (step repo (->Repository {branch (resolve-ref repo ref)} {})))

(defn upsert-branches
  "Upsert multiple branches in one step by supplying a heads map."
  [repo heads]
  (step repo (->Repository (reduce (fn [m [b r]] (assoc m b (resolve-ref repo r))) {} heads) {})))

(defn delete-branch
  "Remove the branch from the repo. This does not remove any blocks."
  [repo branch]
  (step repo (->Repository {branch nil} {})))

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
  (let [blocks1 (map :hash (traverse repo ref1))
        blocks2 (map :hash (traverse repo ref2))]
    (some (apply hash-set blocks1) blocks2)))

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

;; CLEANING

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
        blocks (into {} (map (juxt :hash identity)) (flatten chains))]
    (Repository. (:heads repo) blocks)))


