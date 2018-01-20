(ns blox-machina.core
    (:require [rum.core :as rum]))

(enable-console-print!)

(println "This text is printed from src/blox-machina/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(rum/defc hello-world []
  [:div
   [:h1 (:text @app-state)]
   [:h3 "stuff-man"]])

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
