(ns blox-machina-demo.core
  (:require [blox-machina.blocks :as b]
            [blox-machina.client :as client]
            [blox-machina.projection :as p]
            [rum.core :as rum]))

(enable-console-print!)

(defn string-split-at [n s]
  [(subs s 0 n) (subs s n)])

(defn string-insert [s sub offset]
  (let [[lhs rhs] (string-split-at (- (count s) offset) s)]
    (str lhs sub rhs)))

(defn string-delete [s n offset]
  (let [[lhs rhs] (string-split-at (- (count s) offset n) s)]
    (str lhs (subs rhs n))))

(def builder
  (p/reducing-builder ""
    (fn [s [id data offset]]
      (case id
        :insert (string-insert s data offset)
        :delete (string-delete s data offset)))))

(defn make-pair [] (client/make-pair-client! "origin" {:host "localhost:3001"}))

(let [[*origin *local *disconnect<- *disconnect->] (make-pair)]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *disconnect<- *disconnect<-)
  (def *disconnect-> *disconnect->)
  (def *projection (p/create-projection-client! *chain-origin *chain-local builder))
  #_(p/setup-reflection! *projection
                       (fn [*p reflect-fn]
                         (add-watch *p :rand
                                    (fn [_ _ _ _]
                                      (swap! *chain-local reflect-fn [:push "reflect"]))))))

(def *cursor-offset (atom 0 :validator (complement neg?)))

(defn parse-key [event]
  (let [code (.-keyCode event)]
    (case code
      37 :left-arrow
      39 :right-arrow
      8 :backspace
      (.fromCharCode js/String code))))

(defn event-for-key [c]
  (case c
    :left-arrow [:cursor-left 1]
    :right-arrow [:cursor-right 1]
    :backspace [:delete 1 @*cursor-offset]
    [:insert c @*cursor-offset]))

(defn handle-keyboard-event! [event]
  (let [[id data] event]
    (case id
      :cursor-left (swap! *cursor-offset #(+ % data))
      :cursor-right (swap! *cursor-offset #(- % data))
      (swap! *chain-local b/link-data event))))

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

(rum/defc network-toggles []
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

(rum/defc header < rum/reactive []
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

(defn with-cursor [s cursor-offset cursor-color]
  (let [[lhs rhs] (split-at (- (count s) cursor-offset) s)]
    [:text
     lhs
     [:font {:color cursor-color} "\u2588"]
     (rest rhs)]))

(rum/defcs input-field < rum/reactive
                         (rum/local false ::focused?)
  [state]
  (let [projection (rum/react *projection)
        cursor-offset (rum/react *cursor-offset)
        *focused? (::focused? state)]
    [:div#input-field {:tab-index 0
                       :on-key-down (comp handle-keyboard-event!
                                          event-for-key
                                          parse-key)
                       :on-focus (fn [] (reset! *focused? true))
                       :on-blur (fn [] (reset! *focused? false))
           :style {:min-height "20px"
                   :word-wrap "break-word"
                   :border-style "dashed"
                   :border-color "gray"
                   :border-width "1px"
                   :cursor "pointer"
                   :outline 0}}
     (with-cursor projection cursor-offset (if @*focused? "white" "#444"))]))

(rum/defc hello-world []
  [:div#hello
   (network-toggles)
   (header)
   (input-field)])

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
  )
