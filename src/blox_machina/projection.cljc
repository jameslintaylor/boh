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
           (if-not (empty? delta)
             (swap! *projection build-fn delta)
             (reset! *projection (build-fn @*projection-origin chain-local)))))

       *projection)))

(defn- reflect-fn [*projection *chain]
  (let [*footprint (atom 0)]
    (fn [data]
      (if (not= (:footprint (meta @*chain)) @*footprint)
        (let [footprint (swap! *footprint inc)]
          (swap! *chain #(-> (b/link-data % data)
                             (with-meta {:footprint footprint}))))))))

(defn ^{:style/indent 1} reflect!
  [*projection *chain watch-fn]
  (letfn [(maybe-push! [data]
            (swap! *chain b/link-data data))]
    (watch-fn *projection maybe-push!)))

;; broken
#_(defn create-reflection!
  [*chain-origin *chain-local projector delta-fn]
  (let [*footprint (atom -1)
        *projection (create-projection! *chain-origin *chain-local projector)]
    (add-watch *projection (rand-int 1000)
               (fn [_ _ old new]
                 (when (not= (:footprint (meta new)) @*footprint)
                   (let [delta (delta-fn old new)
                         footprint (reset! *footprint (rand-int 1000))]
                     (swap! *chain-local #(-> %
                                              (b/link-data delta)
                                              (with-meta {:footprint footprint})))))))
    *projection))
