(ns user
  (:require [com.stuartsierra.component :as component]
            [datascript.core :as d]
            [datascript.transit :as dt]
            [figwheel-sidecar.repl-api :as f]))

;; (defonce system nil)

;; (defn start! []
;;   (alter-var-root #'system component/start))

;; (defn stop! []
;;   (alter-var-root #'system component/stop))

;; (defn reset! []
;;   (stop!)
;;   (alter-var-root #'system (constantly (system/make-system))))

;; (defn restart! []
;;   (stop!)
;;   (start!))

;; (defn link-data-and-push! [& data]
;;   (let [head (b/tip @(:*chain (:origin system)))
;;         new-blocks (apply b/create-chain head data)
;;         new-head (b/tip new-blocks)]
;;     (swap! (:*chain (:origin system)) b/link new-blocks)
;;     ((get-in system [:origin :chsk :broadcast-fn])
;;      [:origin/push-blocks {:head new-head
;;                            :blocks new-blocks}])))

