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
;;; (clouchdb:set-connection :name "userdb-64656c6563747573")
;;; (clouchdb:get-all-documents)
;;; (clouchdb:set-connection :name "")
