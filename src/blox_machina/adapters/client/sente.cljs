(ns blox-machina.adapters.client.sente
  (:require [taoensso.sente :as sente]
            [taoensso.sente.packers.transit :as sente-transit]
            [blox-machina.repository-proxy :refer [RepositoryProxy]]
            [blox-machina.transit :refer [transit-readers transit-writers]]
            [cljs.core.async :as a])
  (:require-macros [blox-machina.util :refer [do-with]]))

(defn sente-proxy [host]
  (let [packer (sente-transit/get-transit-packer :json
                                                 {:handlers transit-writers}
                                                 {:handlers transit-readers})
        chsk (sente/make-channel-socket-client! "/chsk" {:host host :packer packer})
        {:keys [ch-recv send-fn state]} chsk]
    (reify RepositoryProxy
      (pull [_ version]
        (do-with [ch (a/chan)]
                 (send-fn [:upstream/pull {:version version}] 1000
                          (partial a/put! ch))))
      (push [_ diff]
        (do-with [ch (a/chan)]
                 (send-fn [:upstream/push {:diff diff}] 1000
                          (partial a/put! ch))))
      (subscribe [_ version]
        (let [xform (comp (filter (comp (partial = :chsk/recv) :id))
                          (map :?data)
                          (filter (comp (partial = :downstream/push) first))
                          (map second)
                          (map :diff))]
          (do-with [ch (a/chan)]
                   (a/pipeline 1 ch xform ch-recv)))))))
