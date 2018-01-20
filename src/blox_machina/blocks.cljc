(ns blox-machina.blocks
  (:require #?(:clj pandect.algo.sha1
               :cljs goog.crypt))
  #?(:cljs (:import goog.crypt.Sha1)))

(defrecord Block [prev-block content hash])

(def sha1
  #?(:clj pandect.algo.sha1/sha1
     :cljs (fn [s]
             (let [inst (goog.crypt.Sha1.)
                   bs (goog.crypt/stringToUtf8ByteArray s)]
               (.update inst bs)
               (goog.crypt/byteArrayToHex (.digest inst))))))

(defn- block-hash [prev-block content]
  (-> (str (pr-str prev-block) (pr-str content))
      sha1
      keyword))

(defn make-block [prev-block content]
  (let [hash (block-hash prev-block content)]
    (Block. prev-block content hash)))

(defn head [blocks] (:hash (last blocks)))
(defn base [blocks] (:prev-block (first blocks)))

(defn chain-contents
  [base & contents]
  (loop [prev-block base
         [h & t] contents
         chain []]
    (if (nil? h) chain
        (let [block (make-block prev-block h)]
          (recur (:hash block) t (conj chain block))))))

(defn rebase
  [chain base]
  (apply chain-contents base (map :content chain)))

(defn link
  [chain & blocks]
  {:pre [(= (head chain) (base blocks))]}
  (into chain blocks))

(defn link-contents
  [chain & contents]
  (let [blocks (apply chain-contents (head chain) contents)]
    (apply link chain blocks)))

(defn tail
  [chain base]
  (->> (rseq chain)
       (take-while #(not (= base (:hash %))))
       reverse
       vec))

(defn- throw-str
  [& strs]
  (let [msg (apply str strs)]
    (throw #? (:clj (Error. msg)
               :cljs (js/Error. msg)))))

(defn verify-blocks
  "Exhaustively checks if a chain make sense by recalculating all the hashes."
  [blocks]
  (reduce (fn [prev-hash block]
            (let [{:keys [prev-block tx-data hash]} block]
              ;; this shortcircuits right?
              (and (or (= hash (block-hash prev-block tx-data))
                       (throw-str "invalid hash: " hash))
                   (or (= prev-hash prev-block)
                       (throw-str "dangling block: " prev-block))
                   hash)))
          :gen blocks))
