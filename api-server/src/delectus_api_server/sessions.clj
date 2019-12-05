(ns delectus-api-server.sessions)


;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; in-memory storage of active user login sessions

(def *sessions* (atom {}))

