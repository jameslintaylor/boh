(ns blox-machina.projection
  (:require [blox-machina.blocks :as b]))

(defn ^{:style/indent 1} reducing-builder [init-value reducing-fn]
  (fn
    ([] init-value)
    ([v [h & t]]
     (if (nil? h)
       v
       (recur (reducing-fn v (:data h)) t)))))

(defn create-projection!
  [*chain build-fn]
  (let [*projection (atom (build-fn (build-fn) @*chain))]
    (b/listen! *chain
      (fn [_ delta]
        (swap! *projection build-fn delta)))
    *projection))

#?(:cljs
   (defn create-projection-client!
     [*chain-origin *chain-local build-fn]
     (let [*projection-origin (create-projection! *chain-origin build-fn)
           *projection (atom (build-fn))]

       (b/listen! *chain-local
         (fn [chain-local delta]
           (if (:reflected? (meta *chain-local))
             (alter-meta! *chain-local assoc :reflected? false)
             (do
               (if (empty? delta)
                 (reset! *projection (build-fn @*projection-origin chain-local))
                 (do
                   (swap! *projection build-fn delta)
                   (alter-meta! *projection assoc :projected? true)))))))

       *projection)))

(defn ^{:style/indent 1} setup-reflection!
  [*projection *chain setup-watch-fn]
  (alter-meta! *projection assoc :projected? false)
  (alter-meta! *chain assoc :reflected? false)
  (letfn [(maybe-reflect! [data]
            (if (-> *projection meta :projected?)
              (alter-meta! *projection assoc :projected? false)
              (do
                (alter-meta! *chain assoc :reflected? true)
                (swap! *chain b/link-data data))))]
    (setup-watch-fn *projection maybe-reflect!)))
