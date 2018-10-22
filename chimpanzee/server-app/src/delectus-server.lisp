;;;; delectus-server.lisp

(in-package #:delectus-server)

(defun connect-opps-daily ()
  (set-connection :name "oppsdaily"))

;;; (get-couchdb-info)
;;; (connect-opps-daily)
;;; (get-document "b449e4da4e28616d4f59f5d5be208a37")
;;; (couchdb-document-properties (get-document "b449e4da4e28616d4f59f5d5be208a37"))
;;; (time (get-all-documents))

(defun document-properties (doc-alist)
  (mapcar #'car doc-alist))

;;; (document-properties (get-document "b449e4da4e28616d4f59f5d5be208a37"))

