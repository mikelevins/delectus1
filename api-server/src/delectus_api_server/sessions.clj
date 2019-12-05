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

(defn make-session [& {:keys [sessionid remote-addr userid timestamp expiration]
                       :or {sessionid nil
                            remote-addr nil
                            userid nil
                            timestamp (System/currentTimeMillis)
                            ;; expires after 1 hour
                            expiration 3600000}}]
  {:sessionid sessionid
   :remote-addr remote-addr
   :userid userid
   :timestamp timestamp
   :expiration expiration})

;;; (make-session)
