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
        (if ((comp :reflected? meta first) delta)
          (println "reflected data, breaking loop")
          (swap! *projection build-fn delta))))
    *projection))

#?(:cljs
   (defn create-projection-client!
     [*chain-origin *chain-local build-fn]
     (let [*projection-origin (create-projection! *chain-origin build-fn)
           *projection (atom (build-fn))]

       (b/listen! *chain-local
         (fn [chain-local delta]
           (if-not (empty? delta)
             (swap! *projection build-fn delta)
             (reset! *projection (build-fn @*projection-origin chain-local)))))

       *projection)))

(defn ^{:style/indent 1} setup-reflection!
  [*projection watch-fn]
  (letfn [(link-reflection! [chain data]
            (if ((comp :projected? meta) chain)
              (println "projected data, breaking loop")
              (let [block (-> (b/create-block (b/head chain) data)
                              (with-meta {:reflected? true}))]
                (b/link chain [block]))))]
    (watch-fn *projection link-reflection!)))
