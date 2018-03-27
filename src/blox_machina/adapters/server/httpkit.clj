(ns blox-machina.adapters.server.httpkit
  (:require [blox-machina.repository-proxy :as rp]
            [blox-machina.transit :refer [transit-readers transit-writers]]
            [clojure.core.async :as a]
            [cognitect.transit :as transit]
            [org.httpkit.server :as httpkit]
            [ring.util.response :refer [response header]]
            [ring.util.io :refer [piped-input-stream]]
            [ring.middleware.transit :refer [wrap-transit-params]]
            [ring.middleware.cors :refer [wrap-cors]]
            [org.httpkit.server :as httpkit]
            [blox-machina.repository :as r]))

;; Need some helpers for asynchronous responses since middleware won't
;; help us :(

(defn my-fill-cors [r]
  (-> r
      (header "Access-Control-Allow-Origin" "http://localhost:3449")
      (header "Access-Control-Allow-Credentials" "true")))

(defn my-transit-response [o]
  (-> (piped-input-stream
       (fn [out]
         (transit/write (transit/writer out :json {:handlers transit-writers}) o)))
      response
      (header "Content-Type" "application/transit+json")))

(defn handle-root! [proxy req]
  (httpkit/with-channel req ch
    (a/go (let [repo (a/<! (rp/pull! proxy {}))]
            (->> (count (:blocks repo))
                 (format "I am a block repository.\n I contain %d blocks.")
                 response
                 (httpkit/send! ch))))))

(defn handle-pull! [proxy req]
  (let [version (get-in req [:transit-params :version])]
    (httpkit/with-channel req ch
      (a/go (let [pull-result (a/<! (rp/pull! proxy version))
                  response (-> (my-transit-response pull-result)
                               my-fill-cors)]
              (httpkit/send! ch response))))))

(defn handle-push! [proxy req]
  (let [diff (get-in req [:transit-params :diff])]
    (httpkit/with-channel req ch
      (a/go (let [push-result (a/<! (rp/push! proxy diff))
                  response (-> (my-transit-response push-result)
                               my-fill-cors)]
              (httpkit/send! ch response))))))

(defn make-root-handler [proxy] (partial handle-root! proxy))
(defn make-pull-handler [proxy] (partial handle-pull! proxy))
(defn make-push-handler [proxy] (partial handle-push! proxy))

(defn handle-routes! [proxy req]
  (case (:request-method req)
    :get  (case (:uri req)
            "/" (handle-root! proxy req)
            (response "not a valid GET endpoint!"))
    :post (case (:uri req)
            "/pull" (handle-pull! proxy req)
            "/push" (handle-push! proxy req)
            (response "not a valid POST endpoint!"))))

(defn make-handler [proxy]
  (let [handler (partial handle-routes! proxy)]
    (-> handler
        (wrap-transit-params {:opts {:handlers transit-readers}})
        (wrap-cors :access-control-allow-origin [#"http://localhost:3449"]
                   :access-control-allow-methods [:post :get :options]
                   :access-control-allow-credentials true))))

(defn run-httpkit-server [proxy port]
  (-> (make-handler proxy)
      (httpkit/run-server {:port port})))
