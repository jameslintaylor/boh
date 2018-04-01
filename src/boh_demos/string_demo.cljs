(ns boh-demos.string-demo
  (:require [rum.core :as rum]
            [boh.repository-reference :as rr]
            [boh.upstream :as u]
            [boh.projection :as p]
            [boh.adapters.client.sente :refer [sente-proxy]]))

(enable-console-print!)

(def remote (sente-proxy "127.0.0.1:3000"))
(defonce *repo (rr/make-ref))
(defonce *branch (atom nil))
(defonce *p (p/projection *repo :-/master str))

(defonce setup
  (do (u/auto-pull-rebase-upstream! *repo remote "origin")
      (u/auto-push-upstream! *repo remote "origin")))

(def counter (atom 0))

(rum/defc root < rum/reactive []
  (let [s (rum/react *p)]
    [:div
     [:input
      {:value (swap! counter inc)
       :on-key-press (fn [e] (->> (.-which e)
                                  (char)
                                  (rr/commit! *repo)))}]
     [:input
]]))

(defn on-js-reload []
  (println "js reloaded"))

(rum/mount (root) (. js/document (getElementById "app")))
