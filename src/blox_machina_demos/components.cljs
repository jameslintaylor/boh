(ns blox-machina-demos.components
  (:require [blox-machina.blocks :as b]
            [rum.core :as rum]))

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

(rum/defc network-toggles
  [*disconnect<- *disconnect->]
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
   (atom-toggle *disconnect->  (line-through "notwork out") "network out")])

(rum/defc header < rum/reactive
  [*chain-origin *chain-local]
  (let [chain-origin (rum/react *chain-origin)
        chain-local (rum/react *chain-local)]
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
        [:font {:color "orange"} "local is at "]
        (take 8 (str (b/head chain-local)))
        [:font {:color "orange"} (str " " (:data (last chain-local)))]])]))
