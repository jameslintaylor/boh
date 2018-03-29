(ns boh.adapters.client.http
  (:require [blox-machina.repository-proxy :refer [RepositoryProxy]]
            [blox-machina.transit :refer [transit-writers transit-readers]]
            [cljs-http.client :as http]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def transit-opts
  {:encoding-opts {:handlers transit-writers}
   :decoding-opts {:handlers transit-readers}})

(defn http-proxy [host & [{:keys [pull-path push-path
                                  subscribe-path]}]]
  (let [make-endpoint (partial str host)
        pull-endpoint (make-endpoint (or pull-path "/pull"))
        push-endpoint (make-endpoint (or pull-path "/push"))]
    (reify RepositoryProxy
      (pull [_ version]
        (http/post pull-endpoint
                   {:channel (a/chan 1 (map :body))
                    :transit-params {:version version}
                    :transit-opts transit-opts}))
      (push [_ diff]
        (http/post push-endpoint
                   {:channel (a/chan 1 (map :body))
                    :transit-params {:diff diff}
                    :transit-opts transit-opts}))
      (subscribe [_ version]
        (println "subscribe is not supported for an http proxy!")
        (a/to-chan [])))))
