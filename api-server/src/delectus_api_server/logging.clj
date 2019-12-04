(ns delectus-api-server.logging
  (:require
   [clojure.pprint :as pp]))

(def *log-requests* (atom false))

(defn enable-logging [](swap! *log-requests* (constantly true)))
(defn disable-logging [](swap! *log-requests* (constantly false)))
(defn logging-enabled? [] @*log-requests*)

(defn wrap-logger [handler]
  (fn [request]
    (let [response (handler request)]
      (when (logging-enabled?)
        (pp/cl-format true "~%request: ~S" request)
        (pp/cl-format true "~%~%response: ~S" response))
      response)))
