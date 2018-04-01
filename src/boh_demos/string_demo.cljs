(ns boh-demos.string-demo
  (:require [rum.core :as rum]
            [boh.repository-reference :as rr]
            [boh.upstream :as u]
            [boh.projection :as p]
            [boh.adapters.client.sente :refer [sente-proxy]]
            [cognitect.transit :as transit]))

(enable-console-print!)

(def remote (sente-proxy "127.0.0.1:3000"))
(defonce *repo (rr/make-ref))
(defonce *branch (atom "master"))

(def read-transit (partial transit/read (transit/reader :json)))
(def write-transit (partial transit/write (transit/writer :json)))

(defn rf
  ([] "")
  ([s] s)
  ([s [id x pos]]
   (case id
     :insert (str (subs s 0 pos) x (subs s pos))
     :delete (if (neg? x)
               (str (subs s 0 (+ pos x)) (subs s pos))
               (str (subs s 0 pos) (subs s (+ pos x))))
     s)))

(defonce *p (p/projection *repo :-/master ((map read-transit) rf)))

(defonce setup
  (do (u/auto-pull-rebase-upstream! *repo remote "origin")
      (u/auto-push-revert-upstream! *repo remote "origin")))

(defn get-target-value [e]
  (.. e -currentTarget -value))

(defn get-cursor-position [e]
  [(.. e -currentTarget -selectionStart)
   (.. e -currentTarget -selectionEnd)])

(def get-text-state (comp (partial apply hash-map)
                          (partial interleave [:text :pos])
                          (juxt get-target-value get-cursor-position)))

(defn left-diff [before after]
  (let [l (- (count before) (count after))]
    (if (pos? l)
      [:delete (- l)]
      [:insert (subs after (+ (count after) l))])))

(defn right-diff [before after]
  (let [l (- (count before) (count after))]
    (if (pos? l)
      [:delete l]
      [:insert (subs after 0 (- l))])))

(defn join-diffs
  [lhs rhs]
  (let [[tl xl pl] lhs
        [tr xr pr] rhs]
    (cond
      (= tl tr :delete) [:delete (+ (- xl) xr) (+ pl xl)]
      (= tl tr :insert) [:insert (str xl xr) pl]
      :else [])))

(defn diff
  "naive split buffer diff."
  [before after]
  (let [[b1 b2] (:pos before)
        [a1 a2] (:pos after)
        [t1 t2] [(:text before) (:text after)]
        lb (subs t1 0 b2)
        la (subs t2 0 a2)
        rb (subs t1 b1)
        ra (subs t2 a1)
        ld (conj (left-diff lb la) b2)
        rd (conj (right-diff rb ra) b1)]
    [ld rd]
    #_(join-diffs ld rd)))

(rum/defc users-list [])

(rum/defcs composition-area < (rum/local nil ::before)
  [state s]
  (let [*before (::before state)]
    [:textarea#compose
     {:value s
      :placeholder "magic"
      :on-click (fn [] (u/pull-rebase-upstream! *repo remote "origin"))
      :on-key-down (comp (partial reset! *before) get-text-state)
      :on-change (fn [e]
                   (let [[ld rd] (diff @*before (get-text-state e))]
                     (rr/commit! *repo (write-transit ld))
                     (rr/commit! *repo (write-transit rd))))}]))

(rum/defc branches-list [])

(rum/defc root < rum/reactive []
  (let [s (rum/react *p)]
    [:div#container
     (users-list)
     (composition-area s)
     (branches-list)]))

(defn on-js-reload []
  (println "js reloaded"))

(rum/mount (root) (. js/document (getElementById "app")))
