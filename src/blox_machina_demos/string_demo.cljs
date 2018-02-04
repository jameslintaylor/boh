(ns blox-machina-demos.string-demo
  (:require [blox-machina-demos.components :as c]
            [blox-machina.blocks :as b]
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

(defn make-pair! [] (client/make-pair-client! "origin" {:host "localhost:3001"}))

(let [[*origin *local *disconnect<- *disconnect->] (make-pair!)]
  (def *chain-origin *origin)
  (def *chain-local *local)
  (def *disconnect<- *disconnect<-)
  (def *disconnect-> *disconnect->)
  (def *projection (p/create-projection-client! *chain-origin *chain-local builder))
  #_(p/setup-reflection! *projection *chain-local
                       (fn [*p reflect!]
                         (add-watch *p :rand
                                    (fn [_ *p old new]
                                      (reflect! [:insert "\n" 0]))))))

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
   (c/network-toggles *disconnect<- *disconnect->)
   (c/header *chain-origin *chain-local)
   (input-field)])

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
  )
