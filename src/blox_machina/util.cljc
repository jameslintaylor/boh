(ns blox-machina.util
  (:require taoensso.sente.interfaces
            [blox-machina.blocks :as b]
            [datascript.core :as d]
            #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])))

(def data-readers (merge d/data-readers
                         {'blox-machina.blocks.Block b/map->Block
                          'blox_machina.blocks.Block b/map->Block}))

(deftype EdnPacker [opts]
  taoensso.sente.interfaces/IPacker
  (pack [_ x] (pr-str x))
  (unpack [_ s] (edn/read-string opts s)))

(def packer (EdnPacker. {:readers data-readers}))
