(ns blox-machina.repository-reference
  (:require [blox-machina.blocks :as b]
            [blox-machina.repository :as r]
            [blox-machina.repository-proxy :as p]
            [blox-machina.util :include-macros true :refer [do-with]]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a])
            [clojure.string :as s])
  #?(:cljs (:require-macros [clojure.core.async.maros :refer [go go-loop]])))

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

(defn swap-step! [ref step-fn & args]
  (swap! ref (fn [repo]
               (let [step (apply step-fn repo args)]
                 ;; strictly speaking swap! fn should be side-effect
                 ;; free :/
                 (broadcast-event! ref :diff (:diff step))
                 (:repo-after step)))))

(defn with-ns
  "adds a namespace to all (keyword) keys in a map."
  [map ns]
  (reduce-kv
   (fn [m k v]
     (let [new-k (if (keyword? k)
                   (keyword ns (name k)))]
       (assoc m new-k v)))
   {} map))

(defn surject-keys [m f & args]
  (reduce-kv (fn [m k v] (assoc m (apply f k args) v)) {} m))

(defn- swap-pull! [ref name diff]
  (let [ns-diff (update diff :branches with-ns (str "-" name))]
    (broadcast-event! ref :pull-diff ns-diff)
    (swap! ref r/merge ns-diff)))

(defn pull-proxy! [ref name proxy]
  (go (let [diff (a/<! (p/pull! proxy (:branches @ref)))]
        (swap-pull! ref name diff))))

(defn push-proxy! [ref proxy])

(defn autosync-proxy! [ref name proxy]
  ;; automatically apply diffs as they are published by the proxy
  (let [sub-ch (p/subscribe! proxy)]
    (go-loop []
      (when-some [diff (a/<! sub-ch)]
        (swap-pull! ref name diff)
        (recur))))
  ;; automatically push diffs as they are swapped! onto the ref
  (let [ch (listen! ref :diff)]
    (go-loop []
      (when-some [diff (a/<! ch)]
        (p/push! proxy diff)
        (recur)))))

(defn as-proxy [ref]
  (reify p/RepositoryProxy
    (pull! [_ version]
      (do-with [ch (a/chan)]
        (a/put! ch (r/diff @ref version))))
    (push! [_ diff]
      (do-with [ch (a/chan)]
        (a/put! ch (:branches (swap-step! ref r/step diff)))))
    (subscribe! [_] (listen! ref :diff))))
