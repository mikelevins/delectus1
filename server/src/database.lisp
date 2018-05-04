;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *database-config-file*
  "/home/delectus/delectus-config.lisp")

(defun initdb ()
  (load *database-config-file*)
  (clouchdb:set-connection :host *dbhost*
                           :port *dbport*
                           :protocol *dbprotocol*
                           :user *dbuser*
                           :password *dbpassword*))

;;; (initdb)
;;; (clouchdb:list-dbs)
;;; (clouchdb:set-connection :name "_users")
;;; (clouchdb:get-all-documents)
