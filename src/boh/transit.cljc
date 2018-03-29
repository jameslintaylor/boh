(ns boh.transit
  (:require [cognitect.transit :as t]
            [blox-machina.repository :as r]
            [blox-machina.blocks :as b])
  #?(:clj (:import [blox_machina.repository Repository]
                   [blox_machina.blocks Block])))

(def transit-writers
  {#?(:clj Repository :cljs r/Repository)
   (t/write-handler
    (constantly "blox/Repository")
    (fn [v] {:heads (:heads v)
             :blocks (:blocks v)}))

   #?(:clj Block :cljs b/Block)
   (t/write-handler
    (constantly "blox/Block")
    (fn [v] [(:prev v) (:data v) (:hash v)]))})

(def transit-readers
  {"blox/Repository"
   (t/read-handler
    (fn [m]
      (r/map->Repository m)))

   "blox/Block"
   (t/read-handler
    (fn [[prev data hash]]
      (b/->Block prev data hash)))})
