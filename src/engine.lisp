;;;; ***********************************************************************
;;;;
;;;; Name:          engine.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus 2 storage engine
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; create-delectus-file ((path pathname))
;;; ---------------------------------------------------------------------
(defmethod create-delectus-file ((path pathname))
  (with-open-database (db path)
    (with-transaction db
      ;; table: delectus - identifies format version
      (execute-non-query db "create table delectus (format_version integer)")
      (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
      ;; table: contents - stores document data
      (execute-non-query db "create table contents (rowid integer primary key, deleted boolean)")
      ;; table: notes - stores user-defined notes about the store document
      (execute-non-query db "create table notes (timestamp, subject, author, note)")
      ;; table: column_order - stores the user-defined column order
      (execute-non-query db "create table column_order (column_name string)")
      (execute-non-query db "insert into column_order (column_name) values (?)" "deleted")
      ;; table: deleted_columns - stores labels of columns that are present but marked deleted
      (execute-non-query db "create table deleted_columns (column_name string)")))
  path)

(defmethod create-delectus-file ((path string))
  (create-delectus-file (pathname path)))

(defmethod ensure-delectus-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           (execute-non-query db "pragma schema_version")
           (execute-non-query db "select * from delectus"))
         path)))

(defmethod ensure-delectus-file ((path string))
  (ensure-delectus-2-file (pathname path)))
