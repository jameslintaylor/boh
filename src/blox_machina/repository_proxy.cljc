(ns blox-machina.repository-proxy
  (:require [blox-machina.blocks :as b]
            [blox-machina.repository :as r]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.core.async :as a :refer [go]]
               :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defprotocol RepositoryProxy
  (pull! [proxy version])
  (push! [proxy diff])
  (subscribe! [proxy]))
