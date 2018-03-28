(ns blox-machina.upstream
  (:require [clojure.string :as s]
            [blox-machina.repository :as r]
            [blox-machina.repository-proxy :as rp]
            [blox-machina.repository-reference :as rr]
            [blox-machina.util :refer [surject-keys #?(:clj go-let)]]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [blox-machina.util :refer [go-let]])))

;; TODO - this should probably live in repository.cljc without the
;; upstream logic.
(defn branch-kw
  "Construct a keyword given a branch name and optionally an upstream
  name(space)."
  ([branch-name]
   (keyword "-" branch-name))
  ([upstream-name branch-name]
   (keyword "-" (str upstream-name "/" branch-name))))

(defn upstream-name [k]
  (-> k name (s/split #"/") first))

(defn branch-name [k]
  (-> k name (s/split #"/") last))

(defn upstream
  "Upstream (namespace) a branch keyword."
  [k upstream-name]
  (branch-kw upstream-name (name k)))

(defn upstream-keys
  "Upstream all branch keys in a branch map."
  [m upstream-name]
  (surject-keys m upstream upstream-name))

(defn normalize
  "Normalize a branch (remove the upstream namespace)."
  [k]
  (branch-kw (branch-name k)))

(defn normalize-keys
  "Normalize all branch keys in a branch map."
  [m]
  (surject-keys m normalize))

(defn upstream? [k]
  (-> k name (s/split #"/") count (> 1)))

(defn normalized-keys
  "Return a seq of the normalized (non-upstream) keys in a branch map."
  [m]
  (into [] (comp (map first) (filter (complement upstream?))) m))

(defn upstreamed-keys
  "Return a seq of the upstreamed branch keys in a branch map.
  Optionally pass an a variable number of upstream names to narrow the
  results."
  ([m] (into [] (comp (map first) (filter upstream?)) m))
  ([m & upstream-names]
   (let [include? (apply hash-set upstream-names)]
     (into [] (comp (map first)
                    (filter (comp include? upstream-name)))
           m))))

(defn select-upstreamed-keys
  [m & upstream-names]
  (->> (apply upstreamed-keys m upstream-names)
       (select-keys m)))

(defn pairs
  "Return pairs of {upstream-name [[upstream-branch branch]...]} in a
  branch-map."
  [m]
  (->> (map (juxt identity normalize) (upstreamed-keys m))
       (group-by (comp upstream-name first))))

(defn rebase-pairs
  "rebase branches of a database onto their qualified and upstream
  counterparts."
  [repo upstream-name]
  (let [pairs (get (pairs (:heads repo)) upstream-name)]
    (reduce (fn [s pair] (apply r/join-step s r/rebase-branch pair))
            (r/identity-step repo) pairs)))

(defn upstream-version
  [m upstream-name]
  (->> (select-upstreamed-keys m upstream-name)
       (normalize-keys)))

(defn- swap-upstream-version!
  [ref version upstream-name]
  (let [upstream-version (upstream-keys version upstream-name)]
    (rr/swap-step! ref :upsteam-version r/upsert-branches upstream-version)))

(defn- swap-upstream-diff!
  [ref diff upstream-name]
  (let [upstream-diff (update diff :heads upstream-keys upstream-name)]
    (if-not (r/adjacent? (:heads @ref) (r/bases upstream-diff))
      (println "refusing to swap in non adjacent upstream diff: "
               upstream-diff)
      (rr/swap-step! ref :upstream-diff r/step upstream-diff))))

(defn pull-upstream!
  "Pull from a named upstream repository."
  [ref proxy upstream-name]
  (go (let [upstream-version (upstream-version (:heads @ref) upstream-name)
            diff (a/<! (rp/pull proxy upstream-version))]
        (swap-upstream-diff! ref diff upstream-name))))

(defn push-upstream!
  "Push to a named upstream repository."
  [ref proxy upstream-name]
  (go (let [{:as repo :keys [heads blocks]} @ref
            upstream-version (upstream-version heads upstream-name)
            normalized-repo (apply r/narrow repo (normalized-keys heads))
            diff (r/diff normalized-repo upstream-version)
            version (a/<! (rp/push proxy diff))]
        (swap-upstream-version! ref version upstream-name))))

(defn pull-rebase-upstream!
  "Pull from a named upstream repository and rebase the paired normal
  branches"
  [ref proxy upstream-name]
  (go (let [_ (a/<! (pull-upstream! ref proxy upstream-name))]
        (rr/swap-step! ref :diff rebase-pairs upstream-name))))

(defn autopull-upstream!
  "Set up ref to automatically pull changes published from the proxy."
  [ref proxy upstream-name]
  (let [upstream-version (upstream-version (:heads @ref) upstream-name)
        ch (rp/subscribe proxy upstream-version)]
    (go-loop []
      (when-some [diff (a/<! ch)]
        (swap-upstream-diff! ref diff upstream-name)
        (recur)))
    ref))
