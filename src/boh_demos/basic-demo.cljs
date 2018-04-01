(ns boh-demos.basic-demo
  (:require [rum.core :as rum]
            [boh.upstream :as u]
            [boh.repository :as r]
            [boh.repository-reference :as rr]
            [boh.projection :as p]
            [boh.adapters.client.http :refer [http-proxy]]
            [boh.adapters.client.sente :refer [sente-proxy]]))

(enable-console-print!)

(def remote (sente-proxy "127.0.0.1:3000"))
(defonce *repo (rr/make-ref))
(defonce *branch (atom nil))
(defonce *p (p/projection *repo :-/master str))

(defn display-hash [hash]
  (if (nil? hash)
    [:span {:style {:color "gray"}} "genesis"]
    [:span {:style {:color "orange"}} (subs (str hash) 0 7)]))

(defn branch-switcher [repo branch]
  (let [branches (u/normalized-keys (:heads repo))]
    [:div#branch-switcher
     [:span
      "on branch "
      [:span#branch-dropdown
       (for [b branches]
         (if (= b branch)
           [:span#active
            (str (u/branch-name (or branch :-)))]
           [:span.hidden
            {:key b
             :on-click (partial reset! *branch b)}
            (subs (str b) 3)]))]]]))

(defn event-value [e]
  (.. e -currentTarget -value))

(rum/defcs block-composer < (rum/local "" ::data)
  [state prev-block branch]
  (let [*data (::data state)]
    [:div#block-composer
     (display-hash (:hash prev-block))
     [:input {:on-change (comp (partial reset! *data) event-value)
              :on-key-up
              (fn [e]
                (let [key (.-which e)]
                  (when (= 13 key)
                    (rr/commit! *repo (or branch :-/master) @*data)
                    (set! (.. e -currentTarget -value) ""))))}]
     [:div#commit-button
      {:on-click (fn [] (rr/commit! *repo (or branch :-/master) @*data))}
      "commit!"]]))

(defn block-element [block upstreams]
  [:div {:key (:hash block)}
   [:div.block-display
    (display-hash (:prev block)) " "
    (:data block) " "
    (display-hash (:hash block))
    (if-not (empty? upstreams)
      [:span.upstream-list "- "
       (for [u upstreams]
         (str (u/upstream-name u) " "))])]])

(defn chain-display [chain branch heads]
  (let [branch-name (u/branch-name (or branch :-))
        upstream-heads (filter (comp (partial = branch-name)
                                      u/branch-name)
                                (u/upstreamed-keys heads))
        upstreams (reduce (fn [m [b h]] (update m h conj b)) {}
                        (select-keys heads upstream-heads))
        [visible hidden] (split-at 10 chain) 
        hidden? (complement (apply hash-set (map :hash visible)))
        hidden-upstreams (flatten
                          (into [] (comp (filter (comp hidden? first))
                                         (map second)
                                         (map (partial map u/upstream-name)))
                                upstreams))]
    [:div#chain-display
        (for [block visible]
          (block-element block (upstreams (:hash block))))
     (if-not (empty? hidden)
       [:div#more-blocks
        (str "... "
             (count hidden)
             " more blocks on "
             (u/branch-name (or branch :-)))
        (if-not (empty? hidden-upstreams)
          [:span.upstream-list "- "
           (for [u hidden-upstreams]
             (str (u/upstream-name u) " "))])])]))

(rum/defc upstream-control []
  [:div#upstream-control
   [:div#upstream-control-buttons
    [:div#pull-button.control-button
     {:on-click (partial u/pull-upstream! *repo remote "origin")}
     "pull"]
    [:div#pull-rebase-button.control-button
     {:on-click (partial u/pull-rebase-upstream! *repo remote "origin")}
     "pull-rebase"]
    [:div#push-button.control-button
     {:on-click (partial u/push-upstream! *repo remote "origin")}
     "push"]
    [:div#push-rebase-button.control-button
     {:on-click (partial u/push-revert-upstream! *repo remote "origin")}
     "push-revert"]
    [:div#autopull-button.control-toggle
     {:on-click (partial u/pull-rebase-upstream! *repo remote "origin")}
     "autopull"]
    [:div#autopush-button.control-toggle
     {:on-click (partial u/pull-rebase-upstream! *repo remote "origin")}
     "autopush"]]
   [:span#upstream-display
    "from/to/with "
    [:span.active
     "origin"]]])

(rum/defc root < rum/reactive []
  (let [{:as repo :keys [heads]} (rum/react *repo)
        branch (rum/react *branch)
        chain (r/traverse repo branch)]
    [:div#container
     [:div#lhs
      [:b "THE REPO"]
      (branch-switcher repo branch)
      (block-composer (first chain) branch)
      (chain-display chain branch heads)]
     [:div#rhs
      [:b "THE UPSTREAMS"]
      (upstream-control)]]))

(defn on-js-reload []
  (println "js reloaded"))

(rum/mount (root) (. js/document (getElementById "app")))
