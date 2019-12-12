(ns delectus-api-server.sessions
  (:require [tick.alpha.api :as t]))


;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; in-memory storage of active user login sessions

(def +sessions+ (atom {}))

(defn clear-sessions! []
  (swap! +sessions+ (constantly {})))

(defn find-session [sessionid]
  (get @+sessions+ sessionid))

(defn add-session! [sessionid session-object]
  (swap! +sessions+
         (fn [ss] (assoc @+sessions+ sessionid session-object)))
  +sessions+)

(defn remove-session! [sessionid]
  (swap! +sessions+
         (fn [ss] (dissoc @+sessions+ sessionid)))
  +sessions+)

;;; +sessions+
;;; (clear-sessions!)
;;; (find-session "1")
;;; (add-session! "1" "session 1")
;;; (remove-session! "1")

(defn make-session [& {:keys [sessionkey remote-addr userid timestamp expiration]
                       :or {sessionkey nil
                            remote-addr nil
                            userid nil
                            timestamp (System/currentTimeMillis)
                            ;; expires after 1 hour
                            expiration 3600000}}]
  {:sessionkey sessionkey
   :remote-addr remote-addr
   :userid userid
   :timestamp timestamp
   :expiration expiration})

;;; (make-session)

(defn session-valid? [request session]
  (let [request-origin (:remote-addr request)
        session-origin (:remote-addr session)]
    (if (= request-origin session-origin)
      (let [timestamp (:timestamp session)
            timestamp-millis (.toEpochMilli (t/parse timestamp))
            expiration (:expiration session)
            expires (+ timestamp-millis expiration)
            now (System/currentTimeMillis)]
        (< now expires))
      false)))

(defn authorized? [request]
  (let [request-session (:session request)
        session-key (:sessionkey request-session)
        active-session (find-session session-key)]
    (if active-session
      (if (session-valid? active-session)
        true
        (do (remove-session! session-key)
            false))
      false)))


