(ns boh.transit
  (:require [cognitect.transit :as t]
            [boh.repository :as r]
            [boh.blocks :as b])
  #?(:clj (:import [boh.repository Repository]
                   [boh.blocks Block])))

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
