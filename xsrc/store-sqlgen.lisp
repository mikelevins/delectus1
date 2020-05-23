;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for generating SQL statements
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; creating tables
;;; ---------------------------------------------------------------------

;;; 'delectus' table
;;; ----------------

(defun sqlgen-create-delectus-table ()
  (yield
   (create-table :delectus
       ((listid :type 'string)
        (format :type 'text)
        (created :type 'integer)
        (modified :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (list-identity
                                   &key
                                     (format +delectus-format-version+)
                                     (created (delectus-timestamp-now))
                                     (modified (delectus-timestamp-now)))
  (yield
   (insert-into :delectus
     (set= :listid list-identity
           :format format
           :created created
           :modified modified))))

;;; (sqlgen-init-delectus-table (makeid))


;;; 'oplog' table
;;; ----------------

(defun sqlgen-create-oplog-table ()
  (yield
   (create-table :oplog
       ((optype :type 'text) ; the type of op: listname, comment, columns, or item
        (timestamp :type 'string) ; the time the op is inserted in the log
        (hash :type 'text) ; the digest of the op data
        (data :type 'text) ; the op data: a JSON object
        ))))

;;; (sqlgen-create-oplog-table)


;;; ---------------------------------------------------------------------
;;; reading and writing ops
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-op (optype timestamp hash data)
  (yield
   (insert-into :oplog
     (set= :optype optype
           :timestamp timestamp
           :hash hash
           :data data))))

;;; (setf $listname "{'name': 'Test'}")
;;; (sqlgen-insert-op "listname" (delectus-timestamp-now) (make-identity-string) $listname)
;;; (setf $comment "{'comment': 'Testing list files'}")
;;; (sqlgen-insert-op "comment" (delectus-timestamp-now) (make-identity-string) $comment)
;;; (setf $columns "{'L78791326b90446889515ce7b9f68977b': {'label': 'L78791326b90446889515ce7b9f68977b', 'name': 'Word'}}")
;;; (sqlgen-insert-op "columns" (delectus-timestamp-now) (make-identity-string) $columns)
;;; (setf $item "{'deleted': false,'values': {'L78791326b90446889515ce7b9f68977b': 'Abraxas'}}")
;;; (sqlgen-insert-op "item" (delectus-timestamp-now) (make-identity-string) $item)
