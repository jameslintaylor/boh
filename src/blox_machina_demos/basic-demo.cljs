(ns blox-machina-demos.basic-demo
  (:require [rum.core :as rum]
            [blox-machina.upstream :as u]
            [blox-machina.repository :as r]
            [blox-machina.repository-reference :as rr]
            [blox-machina.adapters.client.http :refer [http-proxy]]))

(enable-console-print!)

(def remote (http-proxy "http://localhost:3000"))
(defonce *repo (rr/make-ref))
(defonce *branch (atom nil))

(defn display-hash [hash]
  (if (nil? hash)
    [:span {:style {:color "gray"}} "genesis"]
    [:span {:style {:color "orange"}} (subs (str hash) 0 7)]))

(defn block-element [block]
  [:div {:key (:hash block)}
   [:div.block-display
    (display-hash (:prev block)) " "
    (:data block) " "
    (display-hash (:hash block))]])

(defn branch-switcher [repo branch]
  (let [branches (keys (dissoc (:heads repo) branch))]
    [:div
     [:span
      "on branch "
      [:span#branch-dropdown
       [:span#active
        (str branch)]
       (for [b branches]
         [:span.hidden
          {:key b
           :on-click (partial reset! *branch b)}
          (subs (str b) 3)])]]]))

(defn chain-display [blocks]
  [:div#chain-display
   (map block-element blocks)])

(rum/defc hello-strings < rum/reactive []
  (let [repo (rum/react *repo)
        branch (rum/react *branch)]
    [:div#hello
     [:b "A DEMO"]
     (branch-switcher repo branch)
     (chain-display (r/chain repo branch))]))

(rum/mount (hello-strings) (. js/document (getElementById "app")))

(defn on-js-reload []
  )
