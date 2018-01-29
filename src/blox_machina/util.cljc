(ns blox-machina.util
  (:require taoensso.sente.interfaces
            #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            #?(:clj [clojure.core.async :as a]
               :cljs [cljs.core.async :as a])
            #?(:clj pandect.algo.sha1
               :cljs goog.crypt))
  #?(:cljs (:import goog.crypt.Sha1)))

(deftype EdnPacker [opts]
  taoensso.sente.interfaces/IPacker
  (pack [_ x] (pr-str x))
  (unpack [_ s] (edn/read-string opts s)))

(defn edn-packer [opts]
  (EdnPacker. opts))

(def sha1
  #?(:clj pandect.algo.sha1/sha1
     :cljs (fn [s]
             (let [inst (goog.crypt.Sha1.)
                   bs (goog.crypt/stringToUtf8ByteArray s)]
               (.update inst bs)
               (goog.crypt/byteArrayToHex (.digest inst))))))

(defn concat-sha1
  "Concatenates the stringified clojure data and returns its sha1
  hash."
  [& xs]
  (->> (map pr-str xs)
       (apply str)
       sha1
       keyword))

(defn callback-chan!
  "Takes a callback-consuming function and registers with it a callback
  that passes on the values to the returned channel using an optional
  packing function."
  ([register-fn] (callback-chan! register-fn identity))
  ([register-fn packing-fn]
   (let [chan (a/chan)]
     (register-fn (fn [& xs]
                    (a/put! chan (apply packing-fn xs))))
     chan)))

(defn- pipe [in xf]
  (let [out (a/chan)]
    (a/pipeline 4 out xf in)
    out))
