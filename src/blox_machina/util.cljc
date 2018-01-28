(ns blox-machina.util
  (:require taoensso.sente.interfaces
            #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
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
  "Concatenates the stringified clojure data and returns it's sha1
  hash."
  [& xs]
  (->> (map pr-str xs)
       (apply str)
       sha1
       keyword))
