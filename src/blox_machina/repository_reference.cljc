(ns blox-machina.repository-reference
  (:refer-clojure :exclude [proxy-name])
  (:require [blox-machina.repository :as r]
            [blox-machina.repository-proxy :refer [RepositoryProxy]]
            [blox-machina.util :include-macros true :refer [do-with]]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a]))
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

(defn as-proxy [ref]
  (reify RepositoryProxy
    (pull [_ version]
      (do-with [ch (a/chan)]
        (a/put! ch (r/diff @ref version))))
    (push [_ diff]
      (do-with [ch (a/chan)]
        (a/put! ch (:heads (swap-step! ref :diff r/step diff)))))
    (subscribe [_ version] (listen! ref :diff))))
