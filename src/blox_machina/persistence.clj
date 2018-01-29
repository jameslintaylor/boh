(ns blox-machina.persistence
  (:require [clojure.java.io :as io]
            [datascript.core :as d]
            [blox-machina.blocks :as b]))

(defn- tree-path [hash]
  (let [s (subs (str hash) 1)]
    (str (subs s 0 2) "/" (subs s 2))))

(defn- recover-hash [tree-path]
  (keyword (str (subs tree-path 0 2) (subs tree-path 3))))

(defn- serializable [block]
  (let [{:keys [prev-block data hash]} block
        name (tree-path hash)
        contents (pr-str [prev-block data])]
    [name contents]))

(defn serialize-to-fs! [blocks dir]
  (when-not (empty? blocks)
    (doseq [block blocks]
      (let [[name contents] (serializable block)
            path (str dir "/blocks/" name)]
        ;; create intermediate directories if they don't exist
        (io/make-parents path)
        (spit path contents)))
    ;; write head
    (spit (str dir "/head") (tree-path (b/head blocks)))))

(defn- read-block! [tree-path dir]
  (let [s (slurp (str dir "/blocks/" tree-path))
        [prev-block data] (clojure.edn/read-string s)]
    (b/->Block prev-block data (recover-hash tree-path))))

(defn read-chain! [dir]
  (try
    (let [head (slurp (str dir "/head"))]
      (loop [blocks ()
             path head]
        (let [block (read-block! path dir)
              new-blocks (conj blocks block)
              prev-block (:prev-block block)]
          (if-not (= prev-block :gen)
            (recur new-blocks (tree-path prev-block))
            (vec new-blocks)))))
    (catch Exception e [])))
