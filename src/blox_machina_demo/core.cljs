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

(defn make-pair [] (client/make-pair-client! "origin" {:host "localhost:3001"}))

(let [[*origin *local *disconnect<- *disconnect->] (make-pair)]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *disconnect<- *disconnect<-)
  (def *disconnect-> *disconnect->)
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

(rum/defc atom-toggle < rum/reactive
  [atom c-true c-false]
  (let [true? (rum/react atom)]
    [:div {:style {:display "inline-block"
                   :border-style "solid"
                   :border-color "white"
                   :border-width "1px"
                   :padding "4px"
                   :margin "4px"
                   :cursor "pointer"}
           :on-click (fn [] (swap! atom not))}
     (if true? c-true c-false)]))

(rum/defc line-through [c]
  [:div {:style {:text-decoration "line-through"}} c])

(rum/defc hello-world < rum/reactive []
  (let [chain-origin (rum/react *chain-origin)
        chain-local (rum/react *chain-local)
        projection (rum/react *projection)]
    [:div
     [:div#network-toggles {:style {:position "absolute"
                                    :right "10px"
                                    :top "10px"
                                    :float "left"}}
      (atom-toggle *disconnect<- (line-through "network in") "network in")
      [:text {:style {:display "inline block"
                      :cursor "pointer"}
             :on-click (fn []
                         (swap! *disconnect<- not)
                         (swap! *disconnect-> not))}
       "/"]
      (atom-toggle *disconnect->  (line-through "notwork out") "network out")]
     [:div#header {:style {:height "60px"}}
      [:div
       (+ (count chain-origin) (count chain-local))
       [:font {:color "gray"} " blocks in chain"]]
      [:div
       [:font {:color "gray"} "origin is at "]
       (take 8 (str (b/head chain-origin)))
       [:font {:color "gray"} (str " " (:data (last chain-origin)))]]
      (if-not (empty? chain-local)
        [:div
         [:font {:color "gray"} "local is at "]
         (take 8 (str (b/head chain-local)))
         [:font {:color "gray"} (str " " (:data (last chain-local)))]])]
     [:div {:tab-index 0
           :on-key-down (comp push-local
                              event-for-key
                              parse-key)
           :style {:min-height "30px"
                   :border-style "solid"
                   :border-color "white"
                   :border-width "1px"
                   :cursor "pointer"}}
      projection]]))

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
)
