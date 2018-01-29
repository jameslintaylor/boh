(ns blox-machina-demo.core
  (:require [blox-machina.blocks :as b]
            [blox-machina.client :as client]
            [blox-machina.projection :as p]
            [rum.core :as rum]))

(enable-console-print!)

(def builder
  (p/reducing-builder ""
    (fn [s [id data]]
      (case id
        :push (str s data)
        :pop (subs s 0 (- (count s) data))))))

(let [[*origin *local] (client/make-pair-client! "origin" {:host "localhost:3001"})]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *projection (p/create-projection-client! *chain-origin *chain-local builder))
  #_(p/reflect! *projection
    (fn [*p push!]
      (add-watch *p :rand
                 (fn [_ _ old new]
                   (push! (- new old)))))))

(defn parse-key [event]
  (let [code (.-keyCode event)]
    (if (= code 8)
      :backspace
      (char (.-keyCode event)))))

(defn event-for-key [c]
  (if (= c :backspace)
    [:pop 1]
    [:push c]))

(defn push-local [event]
  (swap! *chain-local b/link-data event))

(rum/defc hello-world < rum/reactive []
  (let [chain-origin (rum/react *chain-origin)
        chain-local (rum/react *chain-local)
        projection (rum/react *projection)]
    [:div {:tab-index 0
           :on-key-down (comp push-local
                              event-for-key
                              parse-key)}
     [:div
      (+ (count chain-origin) (count chain-local))
      [:font {:color "gray"} " blocks in chain"]]
     [:div
      [:font {:color "gray"} "origin is at "]
      (take 8 (str (b/head chain-origin)))]
     [:div
      [:font {:color "gray"} "local is at "]
      (take 8 (str (b/head chain-local)))]
     [:h4 projection]]))

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
)
