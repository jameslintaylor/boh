(ns boh.repository-reference
  (:refer-clojure :exclude [proxy-name])
  (:require [boh.repository :as r]
            [boh.repository-proxy :refer [RepositoryProxy]]
            #?(:clj [boh.util :refer [do-with]])
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a])
            [boh.util :refer [surject-keys]])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [boh.util :refer [do-with]])))

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

(defn- -broadcast-event! [ref event-type event-data]
  (a/put! (:pub-chan (meta ref)) {:type event-type
                                  :data event-data}))

;; TODO - is this ok? value comparison on potentially large
;; repositories could be unideal for what should be a simple
;; operation.
(defn meaningful-step [step]
  (let [{:keys [repo-before diff repo-after]} step]
    (if (= repo-before repo-after)
      nil step)))

;; `-swap-step!` and `swap-step!` implementations basically just copy
;; datascript's transact implementations.

(defn- -swap-step! [ref step-fn & args]
  (let [*step (atom nil)]
    (swap! ref (fn [repo]
                 (let [s (apply step-fn repo args)]
                   (reset! *step s)
                   (:repo-after s))))
    @*step))

(defn swap-step! [ref event-type step-fn & args]
  (let [step (apply -swap-step! ref step-fn args)]
    (when-some [step (meaningful-step step)]
      (-broadcast-event! ref event-type step))
    step))

(defn commit!
  ([ref data] (commit! ref :-/master data))
  ([ref branch data]
   (swap-step! ref :step r/commit branch data)))

(defn ordered-merge-fn
  "Combine multiple merge strategies into one by taking the result of
  the first merge strategy that does not return nil."
  [& merge-fns]
  (fn [repo diff]
    (loop [[merge-fn & t] merge-fns]
      (if (nil? merge-fn)
        nil
        (if-some [step (merge-fn repo diff)]
          step
          (recur t))))))

(defn fastforward-merge [repo diff]
  (when (r/adjacent? (:heads repo) (r/bases diff))
    (println "fastforward merge!")
    (r/step repo diff)))

(defn tmp-branch [k]
  (keyword "-" (str "~" (name k))))

(defn rebase-merge [repo diff]
  (let [bases (r/bases diff)]
    (when (r/includes? repo bases)
      (println "rebasing merge!")
      (let [tmp-diff (update diff :heads surject-keys tmp-branch)
            pairs (map vector (keys bases) (keys (:heads tmp-diff)))
            ;; start with a step that contains blocks since branch point
            initial (-> (r/step repo (r/diff repo bases))
                        (r/join-step r/step tmp-diff))]
        ;; merge the temporary branches
        (reduce (fn [s p]
                  (apply r/join-step s r/rebase-merge-branch p))
                initial pairs)
        ;; TODO: - prune here
        ))))

(defn refuse-merge [repo _]
  (println "refusing merge!")
  (r/identity-step repo))

(defn reasonable-merge-fn
  "Generate a merge function following a reasonable merge strategy.
  Tries first to fastforward, then rebase, and finally refuse if
  neither could be accomplished."
  []
  (ordered-merge-fn fastforward-merge
                    rebase-merge
                    refuse-merge))

(defn as-proxy
  "Return a reification of `RepositoryProxy` implementing an optional
  merge function. If no merge function is specified, will just use a
  basic fast-forward only merge strategy."
  ([ref] (as-proxy ref (reasonable-merge-fn)))
  ([ref merge-fn]
   (reify RepositoryProxy
     (pull [_ version]
       (do-with [ch (a/chan)]
         (a/put! ch (r/diff @ref version))))
     (push [_ diff]
       (do-with [ch (a/chan)]
         (a/put! ch (:diff (swap-step! ref :step merge-fn diff)))))
     (subscribe [_ version]
       (do-with [ch (a/chan)]
         (a/pipeline 1 ch (map :diff) (listen! ref :step)))))))
