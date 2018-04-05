(ns boh-demos.handlers)

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

(def wrap-keycode
  (map (fn [e]
         (let [{:keys [dom-event]} e]
           (assoc e :keycode (keycode dom-event))))))

(def wrap-target-value
  (map (fn [e]
         (let [{:keys [dom-event]} e]
           (assoc e :target-value (target-value dom-event))))))

(defn- -wrap-sig [hf]
  (fn [dom-event]
    (hf nil {:dom-event dom-event})))

(defn handler [& fs]
  ((apply comp (conj (drop-last fs) -wrap-sig))
   (fn [_ e]
     ((last fs) e))))
