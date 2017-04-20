;;;; ***********************************************************************
;;;;
;;;; Name:          store.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)


;;; ---------------------------------------------------------------------
;;; system-column-labels
;;; ---------------------------------------------------------------------
;;; columns that every delectus contents table possesses, regardless
;;; of what columns a user supplies.

(defparameter +system-column-labels+
  '("rowid" "deleted"))

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
(defclass store ()
  ;; pathname of a SQLite3 file with appropriate Delectus tables
  ((data-path :accessor data-path :initform nil :initarg :data-path)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))

(defmethod store-column-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (append +system-column-labels+
              (mapcar #'first (execute-to-list db "select * from column_order"))))))

(defmethod store-deleted-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (execute-to-list db "select * from deleted_columns"))))

(defmethod store-nondeleted-rows ((store store) &optional (column-labels nil))
  (with-open-database (db (data-path store))
    (with-transaction db
      (if column-labels
          (execute-to-list db (format nil "select ~{~s~^, ~} from contents where deleted = 0"
                                      column-labels))
        (execute-to-list db "select * from contents where deleted = 0")))))



