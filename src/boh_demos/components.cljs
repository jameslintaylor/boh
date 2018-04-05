(ns boh-demos.components
  (:require [boh.blocks :as b]
            [rum.core :as rum]))

(defn target-value [e]
  (.. e -currentTarget -value))

(defn cursor-position [e]
  [(.. e -currentTarget -selectionStart)
   (.. e -currentTarget -selectionEnd)])

(defn keycode [e]
  (.-which e))

(defn prevent-default [e]
  (.preventDefault e)
  e)

(defn ensure-attr [[t ?a & c]]
  (if (map? ?a)
    (apply vector t ?a c)
    (apply vector t {} ?a c)))

(defn attr [cp k v]
  (let [[t a & c] (ensure-attr cp)]
    (apply vector t (assoc a k v) c)))

(defn attrs [cp kvs]
  (let [[t a & c] (ensure-attr cp)]
    (apply vector t (into a kvs) c)))

(defn in-attr [cp path v]
  (let [[t a & c] (ensure-attr cp)]
    (apply vector t (assoc-in a path v) c)))

(defn class [cp classes]
  (attr cp :class classes))

(defn on-change-set [cp *a path]
  (letfn [(on-change [e]
            (let [new (target-value e)]
              (swap! *a assoc-in path new)))]
    (attr cp :on-change on-change)))

(defn on-key-down [cp f]
  (attr cp :on-key-down f))

(defn wrap-filter-keycodes [f pred]
  (fn [e]
    (let [keycode (keycode e)]
      (when (pred keycode)
        (f e)))))

(defn on-enter [cp f]
  (on-key-down cp (wrap-filter-keycodes f #{13})))

(defn on-click [cp f]
  (attr cp :on-click f))

(defn on-mouse-enter [cp f]
  (attr cp :on-mouse-enter f))

(defn on-mouse-out [cp f]
  (attr cp :on-mouse-out f))

(defn style [cp k v]
  (in-attr cp [:style k] v))

(defn in-container [cp t]
  [t cp])

(defn with-first-child [cp c]
  (let [[t a & cs] (ensure-attr cp)]
    (apply vector t a c cs)))

(defn tooltip [cp text]
  (-> cp
      (in-container :div.tooltip)
      (with-first-child [:span.tooltiptext text])))
