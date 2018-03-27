(ns blox-machina.repository-reference
  (:refer-clojure :exclude [proxy-name])
  (:require [blox-machina.blocks :as b]
            [blox-machina.repository :as r]
            [blox-machina.repository-proxy :as p]
            [blox-machina.util :include-macros true :refer [do-with with-ns surject-keys]]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a])
            [clojure.string :as s])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn make-ref
  ([] (make-ref (r/empty-repo)))
  ([repo]
   (let [pub-chan (a/chan)]
     (atom repo :meta {:pub-chan pub-chan
                       :pub (a/pub pub-chan :type)}))))

(defn listen! [ref & event-types]
  (let [event-chans (map vector event-types (repeatedly (partial a/chan)))
        out-chan (a/chan)]
    (doseq [[event-type sub-chan] event-chans]
      (a/sub (:pub (meta ref)) event-type sub-chan))
    (a/pipeline 1 out-chan (map :data) (a/merge (map second event-chans)))
    out-chan))

(defn- broadcast-event! [ref event-type event-data]
  (a/put! (:pub-chan (meta ref)) {:type event-type
                                  :data event-data}))

;; :/
(defn meaningful-diff [step]
  (let [{:keys [repo-before diff repo-after]} step]
    (if (= repo-before repo-after)
      nil diff)))

(defn swap-step! [ref event-type step-fn & args]
  (swap! ref (fn [repo]
               (let [step (apply step-fn repo args)]

                 ;; strictly speaking swap! fn should be side-effect
                 ;; free :/
                 ;;
                 ;; also probably need a better way for comparing
                 ;; these to determine if a diff should be pushed.
                 (when-some [diff (meaningful-diff step)]
                   (broadcast-event! ref event-type diff))

                 (:repo-after step)))))

(defn commit!
  ([ref data] (commit! ref :-/master data))
  ([ref branch data]
   (swap-step! ref :diff r/commit branch data)))

(defn branch-keyword
  ([branch-name]
   (keyword "-" branch-name))
  ([proxy-name branch-name]
   (keyword "-" (str proxy-name "/" branch-name))))

(defn with-proxy-ns [branches proxy-name]
  (surject-keys branches (comp (partial branch-keyword proxy-name) name)))

(defn proxy-ns [branch-keyword]
  ((comp namespace keyword name) branch-keyword))

(defn branch-name [branch-keyword]
  (last (s/split (name branch-keyword) #"/")))

(defn rebase-unq
  "rebase the unqualified branches (typically local) branches of a
  database onto their ns qualified counterparts (typcally remote)."
  [repo proxy-name]
  (let [branches (:branches repo)
        upstream-keys (filter (comp (partial = proxy-name) proxy-ns)
                              (keys branches))
        pairs (map (fn [b] [b (branch-keyword (branch-name b))])
                   upstream-keys)]
    (reduce (fn [s pair] (apply r/join-step s r/rebase-branch pair))
            (r/->RepositoryStep repo {:branches {} :blocks {}} repo) pairs)))

(defn- swap-pull! [ref name diff]
  (let [{:keys [blocks branches]} diff
        ns-branches (with-proxy-ns branches name)]
    (swap-step! ref :proxy-diff r/upsert-branches ns-branches)
    (swap-step! ref :diff r/add-blocks blocks)
    (swap-step! ref :diff rebase-unq name)))

(defn pull-proxy! [ref name proxy]
  (go (let [diff (a/<! (p/pull! proxy (:branches @ref)))]
        (swap-pull! ref name diff))))

(defn autopull-proxy!
  "Set up ref to automatically pull changes published from the proxy."
  [ref name proxy]
  (let [ch (p/subscribe! proxy)]
    (go-loop []
      (when-some [diff (a/<! ch)]
        (swap-pull! ref name diff)
        (recur)))
    ref))

(defn autopush-proxy!
  "Set up ref to attempt to automatically push changes to the proxy."
  [ref name proxy]
  (let [ch (listen! ref :diff)]
    (go-loop []
      (when-some [diff (a/<! ch)]
        (p/push! proxy diff)
        (recur)))
    ref))

(defn autosync-proxy! [ref name proxy]
  (-> ref
      (autopull-proxy! name proxy)
      (autopush-proxy! name proxy)))

(defn as-proxy [ref]
  (reify p/RepositoryProxy
    (pull! [_ version]
      (do-with [ch (a/chan)]
        (a/put! ch (r/diff @ref version))))
    (push! [_ diff]
      (do-with [ch (a/chan)]
        (a/put! ch (:branches (swap-step! ref :diff r/step diff)))))
    (subscribe! [_] (listen! ref :diff))))
