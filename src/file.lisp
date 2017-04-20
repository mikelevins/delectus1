;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       managing Delectus store files
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; store files
;;; ---------------------------------------------------------------------
;;; a delectus store file is a SQLite3 file with the following tables:
;;;
;;; - delectus (format_version) 
;;;   the format_version column contains the version number of the
;;;   file format. The version format is semantic versioning i.j.k...
;;;   with as many numbers i,j,k as needed in subsequent rows.
;;;
;;; - contents (rowid, deleted, label*)
;;;   where rowid is a monotonic unique index, deleted is a boolean
;;;   row marker, and label* is user-supplied columns
;;;
;;; - notes (timestamp, subject, author, note)
;;;   contains arbitrary user-supplied notes marked with a
;;;   timestamp and an optional subject and author
;;; 
;;; - column_order (column_name string)
;;;   lists the user-defined column labels in user-defined order.  by
;;;   default the columns appear in order of creation, but users can
;;;   reorder them. views always fetch columns in the order recorded
;;;   in column_order.
;;;
;;; - deleted_columns (column_name string)
;;;   the labels of columns marked deleted, one per row. empty by
;;;   default. When a user marks a column deleted it is added to
;;;   this table. If a column is undeleted or permanently deleted
;;;   (that is, dropped from the table) its label is removed from
;;;   deleted_columns
;;;
;;;   the system columns and the deleted columns are together termed
;;;   *** hidden columns ***. 
;;;
;;;   *** visible columns *** are all columns except the hidden
;;;   columns.

;;; create-delectus-file ((path pathname))
;;; ---------------------------------------------------------------------
;;; 
(defmethod create-delectus-file ((path pathname) &optional (column-labels nil)(deleted-labels nil))
  (assert (equalp column-labels (remove-duplicates column-labels :test #'equal))()
    "Duplicate column labels in ~S" column-labels)
  ;; remove "rowid" and "deleted" from user-supplied columns; we handle them specially
  (let ((column-labels (remove "rowid" (remove "deleted" column-labels :test #'equal) :test #'equal)))
    (with-open-database (db path)
      (with-transaction db
        ;; table: delectus - identifies format version
        (execute-non-query db "create table delectus (format_version integer)")
        (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
        ;; table: contents - stores document data
        ;;        columns are (rowid deleted [user-supplied column labels])
        (if column-labels
            (execute-non-query db (format nil
                                          "create table contents (rowid integer primary key, deleted boolean not null default 0, ~{~s~^, ~})"
                                          column-labels))
          (execute-non-query db "create table contents (rowid integer primary key, deleted boolean not null default 0)"))
        ;; table: notes - stores user-defined notes about the store document
        (execute-non-query db "create table notes (timestamp, subject, author, note)")
        ;; table: column_order - stores the user-defined column order
        (execute-non-query db "create table column_order (column_name string)")
        (dolist (lbl column-labels)
          (execute-non-query db "insert into column_order (column_name) values (?)" lbl))
        ;; table: deleted_columns - stores labels of columns that are present but marked deleted
        (execute-non-query db "create table deleted_columns (column_name string)")
        (dolist (lbl deleted-labels)
          (execute-non-query db "insert into deleted_columns (column_name) values (?)" lbl)))))
  path)

(defmethod create-delectus-file ((path string) &optional (column-labels nil)(deleted-labels nil))
  (create-delectus-file (pathname path) column-labels deleted-labels))

(defmethod ensure-delectus-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           (execute-non-query db "pragma schema_version")
           (execute-non-query db "select * from delectus"))
         path)))

(defmethod ensure-delectus-file ((path string))
  (ensure-delectus-file (pathname path)))
