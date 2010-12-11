;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite.lisp
;;;; Project:       Delectus 2
;;;; Purpose:        Delectus SQLite interface
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod make-column-description ((label string) type &key (primary-key nil)(nullable t)
                               &allow-other-keys)
  (let* ((typestr (ecase type
                    ((:text) "text")
                    ((:integer) "integer")))
         (primstr (if primary-key "primary key" ""))
         (nullstr (if nullable "null" "not null")))
    (format nil "~A ~A ~A ~A" label typestr primstr nullstr)))

;;; (make-column-description "index" :integer :primary-key t :nullable nil)

(defun make-memory-db (&rest labels)
  (let* ((cols (cons (make-column-description "id" :integer :primary-key t :nullable nil)
                    (mapcar (^ (lbl)(make-column-description lbl :text))
                            labels)))
         (delectus-stmt (format nil "create table delectus (~{~A~^, ~})" cols))
         (create-deleted-cols-stmt (format nil "create table deleted_columns (column_labels)"))
         (create-deleted-rows-stmt (format nil "create table deleted_rows (row_indexes)"))
         (db (connect ":memory:")))
    (execute-non-query db delectus-stmt)
    (execute-non-query db create-deleted-cols-stmt)
    (execute-non-query db create-deleted-rows-stmt)
    db))

;;; (time (setq $db (make-memory-db)))
;;; (time (setq $db (make-memory-db "zips" "cities" "latitudes" "longitudes" "dst")))
;;; (execute-to-list $db "PRAGMA table_info('delectus')")
;;; (disconnect $db)

(defmethod save-memory-db ((db sqlite-handle))
  ;; collect the table info from the memory db
  ;; create a new disk db with the same tables
  ;; insert data from the memory db into the disk db
  ;; disconnect and abandon the memory db
  )
