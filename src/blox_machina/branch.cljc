(ns blox-machina.branch
  (:require [blox-machina.blocks :as b]
            #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defprotocol ChainProxy
  "A means to indirectly control a chain."
  (pull-result [this base])
  (push-result [this chain]))

(defprotocol ChainProxyAsync
  "A means to indirectly control a chain asynchronously."
  (ch-pull-result [this base])
  (ch-push-result [this chain]))

(defprotocol ChainRecv
  (ch-recv [this]))

(defn create-branch
  ([] (apply create-branch (repeat 2 (b/create-chain :gen))))
  ([upstream-chain branch-chain]
   {:upstream-chain upstream-chain
    :branch-chain branch-chain}))

(defn base [branch]
  (b/base (:upstream-chain branch)))

(defn head [branch]
  (b/head (:branch-chain branch)))

(defn upstream-head [branch]
  (b/head (:upstream-chain branch)))

(defn flatten-chain [branch]
  (b/link (:upstream-chain branch) (:branch-chain branch)))

(defn commit [branch & data]
  (update branch :branch-chain #(apply b/link-data % data)))

(defn forward-upstream [branch chain]
  {:pre [(b/adjacent? (:upstream-chain branch) chain)]}
  (-> branch
      (update :upstream-chain b/link chain)
      (update :branch-chain b/rebase (b/head chain))))

(defn transfer-upstream [branch chain]
  {:pre [(= (b/base chain) (upstream-head branch))]}
  (-> branch
      (update :upstream-chain b/link chain)
      (update :branch-chain b/chain-since (b/head chain))))

(defn with-pull [branch result]
  (let [{:keys [head chain]} result]
    (forward-upstream branch chain)))

(defn with-push [branch chain result]
  (let [{:keys [head ?forward-chain]} result]
    (if (= head (b/head chain))
      ;; if the result head is the same as the branch head, everything
      ;; went perfectly.
      (transfer-upstream branch chain)
      ;; otherwise, forward the chain and discard the branch.
      (do
        (-> branch
            (update :branch-chain b/chain-since (b/head chain))
            (forward-upstream ?forward-chain))))))

(defn pull! [branch upstream-proxy]
  {:pre [(satisfies? ChainProxy upstream-proxy)]}
  (let [result (pull-result upstream-proxy (upstream-head branch))]
    (with-pull branch result)))

(defn pull-async! [branch upstream-proxy]
  {:pre [(satisfies? ChainProxyAsync upstream-proxy)]}
  (let [ch (a/chan)]
    (go
      (let [result (a/<! (ch-pull-result upstream-proxy (upstream-head branch)))]
        (a/>! ch (with-pull branch result))))
    ch))

(defn push! [branch upstream-proxy]
  {:pre [(satisfies? ChainProxy upstream-proxy)]}
  (let [{:keys [branch-chain]} branch
        result (push-result upstream-proxy branch-chain)]
    (with-push branch branch-chain result)))

(defn push-async! [branch upstream-proxy]
  {:pre [(satisfies? ChainProxyAsync upstream-proxy)]}
  (let [ch (a/chan)
        {:keys [branch-chain]} branch]
    (go
      (let [push-result (a/<! (ch-push-result upstream-proxy branch-chain))]
        (a/>! ch (with-push branch branch-chain push-result))))
    ch))

(defn swap-async! [atom f & args]
  (a/take! (apply f @atom args) (partial reset! atom)))

(defn ch-swap! [atom f & args]
  (let [ch (a/chan)]
    (go
      (when-let [v (a/<! (apply f @atom args))]
        (a/>! ch (reset! atom v))))
    ch))

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
      (println "pushing")
      (when-not (empty? (:+  branch))
        (swap! conn push! proxy)))))

(defn auto-push-async! [conn proxy]
  {:pre [(satisfies? ChainProxyAsync proxy)]}
  ;; keep some serial semantics by only allowing one "outgoing" push
  ;; at a time.
  (let [*outgoing? (atom false)]
    (listen! conn
      (fn [branch {:keys [upstream branch]}]
        (when-not (or (empty? (:+ branch)) @*outgoing?)
          (reset! *outgoing? true)
          (go
            (a/<! (ch-swap! conn push-async! proxy))
            (reset! *outgoing? false)))))))

(defn auto-recv! [conn recv]
  {:pre [(satisfies? ChainRecv recv)]}
  (let [ch-diff (ch-recv recv)]
    (go-loop []
      (when-let [chain (:+ (a/<! ch-diff))]
        (try (swap! conn forward-upstream chain)
             (catch AssertionError e (println "oops")))
        (recur)))
    (fn []
      (a/close! ch-diff))))

(defn auto-track! [conn upstream-proxy-recv]
  (juxt (auto-push! conn upstream-proxy-recv)
        (auto-recv! conn upstream-proxy-recv)))

(defn auto-track-async! [conn upstream-proxy-recv]
  (juxt (auto-push-async! conn upstream-proxy-recv)
        (auto-recv! conn upstream-proxy-recv)))

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
  (ch-recv [this]
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

(defn- lift-proxy
  "lifts a synchronous `ChainProxy` implementation into one that
  provides the asynchronous `ChainProxyAsync`."
  [proxy]
  {:pre [(satisfies? ChainProxy proxy)]}
  (reify

    ChainProxyAsync
    (ch-pull-result [this base]
      (let [ch (a/chan)]
        (a/put! ch (pull-result proxy base))
        ch))
    (ch-push-result [this chain]
      (let [ch (a/chan)]
        (a/put! ch (push-result proxy chain))
        ch))

    ChainRecv
    (ch-recv [this]
      (ch-recv proxy))))
