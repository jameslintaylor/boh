(ns blox-machina.branch
  (:require [blox-machina.blocks :as b]
            #?(:clj [clojure.core.async :as a]
               :cljs [cljs.core.async :as a])))

(defprotocol ChainProxy
  "A means to indirectly control a chain."
  (pull-result [this base])
  (push-result [this chain]))

(defprotocol ChainRecv
  (ch-recv! [this]))

(defn create-branch
  ([] (apply branch (repeat 2 (b/create-chain :gen))))
  ([upstream-chain branch-chain]
   {:upstream-chain upstream-chain
    :branch-chain branch-chain}))

(defn flatten-chain [branch]
  (b/link (:upstream-chain branch) (:branch-chain chain)))

(defn commit [branch & data]
  (update branch :branch-chain #(apply b/link-data % data)))

(defn forward-upstream [branch chain]
  {:pre [(= (b/head (:upstream-chain branch)) (b/base chain))]}
  (-> branch
      (update :upstream-chain b/link chain)
      (update :branch-chain b/rebase (b/head chain))))

(defn transfer-upstream [branch chain]
  {:pre [(= (b/head (:upstream-chain branch)) (b/base chain))]}
  (-> branch
      (update :upstream-chain b/link chain)
      (update :branch-chain b/chain-since (b/head chain))))

(defn pull! [branch proxy]
  {:pre [(satisfies? ChainProxy proxy)]}
  (let [{:keys [upstream-chain branch-chain]} branch
        {:keys [head chain]} (pull-result proxy (b/head upstream-chain))]
    (forward-upstream branch chain)))

(defn push! [branch proxy]
  {:pre [(satisfies? ChainProxy proxy)]}
  (let [{:keys [upstream-chain branch-chain]} branch
        {:keys [head ?forward-chain]} (push-result proxy branch-chain)]
    (if (= head (b/head branch-chain))
      ;; if the result head is the same as the branch head, everything
      ;; went perfectly.
      (transfer-upstream branch branch-chain)
      ;; otherwise, some patching was involved.
      (-> branch
          (update :branch-chain b/chain-since (b/head branch-chain))
          (forward-upstream branch ?forward-chain)))))

(defn create-conn []
  (atom (create-branch)))

(defn diff [branch-old branch-new]
  {:upstream (b/diff (:upstream-chain branch-old)
                     (:upstream-chain branch-new))
   :branch (b/diff (:branch-chain branch-old)
                   (:branch-chain branch-new))})

(defn ^{:style/indent 1} listen! [conn listen-fn]
  (let [key (rand-int 1000)]
    (add-watch conn key
               (fn [_ _ old new]
                 (listen-fn new (diff old new))))
    (fn [] (remove-watch conn key))))

(defn auto-push! [conn proxy]
  {:pre [(satisfies? ChainProxy proxy)]}
  (listen! conn
    (fn [branch {:keys [upstream branch]}]
      (when-not (empty? (:+  branch))
        (swap! conn push! proxy)))))

(defn auto-recv! [conn recv]
  {:pre [(satisfies? ChainRecv recv)]}
  (let [ch-diff (ch-recv! recv)]
    (go-loop []
      (let [diff (a/<! ch-diff)]
        (swap! conn forward-upstream (:+ diff))
        (recur)))
    (fn []
      (a/close! ch-diff))))

(defn auto-sync! [conn proxy-recv]
  (let [stop-push! (auto-push! conn proxy-recv)
        stop-recv! (auto-recv! conn proxy-recv)]
    (fn []
      (stop-push!)
      (stop-recv!))))

(defrecord ChainRef [*chain]

  ChainProxy
  (pull-result [this base]
    (let [chain @*chain]
      {:head (b/head chain)
       :chain (b/chain-since chain base)}))

  (push-result [this new-chain]
    (let [chain @*chain]
      (if (= (b/base new-chain) (b/head chain))
        {:head (b/head (swap! *chain b/link new-chain))}
        (let [missing-chain (b/chain-since chain (b/base new-chain))
              rebased-chain (b/rebase new-chain (b/head missing-chain))]
          {:head (b/head (swap! *chain b/link rebased-chain))
           :?forward-chain (b/link missing-chain rebased-chain)}))))

  ChainRecv
  (ch-recv! [this]
    (let [ch (a/chan)]
      (b/listen! *chain
        (fn [_ diff]
          (a/put! ch diff)))
      ch)))

(defn chain-ref [*chain]
  (->ChainRef *chain))

(defn sink []
  (reify ChainProxy
    (pull-result [this base]
      (println "nothing can be retrieved from the sink!"))
    (push-result [this chain]
      {:head (b/head chain)})))
