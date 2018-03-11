;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       managing Delectus store files
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.file)

;;; =====================================================================
;;; Store Files
;;; =====================================================================
;;;
;;; A delectus store file is a SQLite3 file with the following tables:
;;;
;;; - delectus (keys, vals) 
;;;   The `delectus` table is a property map that stores metadata
;;;   describing the file as key/value pairs. Two keys present
;;;   in all files are "delectus.version", the version of the 
;;;   Delectus application that created the file, and 
;;;   "delectus.format.version", the version of the Delectus
;;;   file format used in the file.
;;;
;;; - contents (rowid, label*)
;;;   `rowid` is a monotonic unique index and `label*` is user-supplied
;;;   column labels.
;;;
;;; - notes (timestamp, subject, author, note)
;;;   Contains arbitrary user-supplied notes marked with a timestamp
;;;   and an optional subject and author.
;;; 
;;; - column_order (column_name string)
;;;   Lists contents' user-defined column labels in user-defined
;;;   order. It is an invariant of Delectus that the (hidden) `rowid`
;;;   column is always the first column. By default the remaining
;;;   columns appear in order of creation, but users can reorder them
;;;   (except that, of course, no column can be placed before
;;;   `rowid`). Views always fetch columns in the order recorded in
;;;   `column_order`.

;;; ---------------------------------------------------------------------
;;; create-delectus-file (path &optional (column-labels nil))
;;; ---------------------------------------------------------------------
;;; *exported generic function* 
;;;
;;; Returns the pathname of a newly-created Delectus store file.
;;; Creates the store file and populates it with the required tables.
;;; Signals an error if the file cannot be created, if elements of
;;; COLUMN-LABELS are duplicates, or if reserved labels are passed in
;;; COLUMN-LABELS.

(defmethod create-delectus-file ((path pathname) &optional (column-labels nil))
  (assert (equalp column-labels (remove-duplicates column-labels :test #'equalp))()
          "Duplicate column labels in ~S" column-labels)
  (assert (every #'delectus.system:valid-column-label? column-labels) ()
          "The labels ~S are reserved for Delectus and cannot be used for columns." delectus.system:+reserved-column-labels+)
  (with-open-database (db path)
    (with-transaction db
      ;; table: delectus - identifies format version
      (execute-non-query db "create table delectus (keys, vals)")
      (execute-non-query db "insert into delectus (keys, vals) values (?,?)" 
                         "delectus.version" (delectus.version:delectus-version-string))
      (execute-non-query db "insert into delectus (keys, vals) values (?,?)" 
                         "delectus.format.version"
                         (format nil "~A.~A" 
                                 delectus.version:+delectus-format-major-version+
                                 delectus.version:+delectus-format-minor-version+))
      ;; table: contents - stores document data
      ;;        columns are (rowid [user-supplied column label]*)
      (if column-labels
          (execute-non-query db (format nil "create table contents (rowid INTEGER PRIMARY KEY, ~{~s~^, ~})" column-labels))
          (execute-non-query db "create table contents (rowid INTEGER PRIMARY KEY)"))
      ;; table: notes - stores user-defined notes about the store document
      (execute-non-query db "create table notes (timestamp, subject, author, note)")
      ;; table: column_order - stores the user-defined column order
      (execute-non-query db "create table column_order (column_name string)")
      (dolist (lbl column-labels)
        (execute-non-query db "insert into column_order (column_name) values (?)" lbl))))
  path)

(defmethod create-delectus-file ((path string) &optional (column-labels nil))
  (create-delectus-file (pathname path) column-labels))

;;; ---------------------------------------------------------------------
;;; probe-delectus-file (path)
;;; ---------------------------------------------------------------------
;;; *exported generic function* 
;;;
;;; returns true if PATH is the pathname of an existing Delectus file and 
;;; NIL otherwise.

(defmethod probe-delectus-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           (execute-non-query db "pragma schema_version")
           (execute-non-query db "select * from delectus"))
         path)))

(defmethod probe-delectus-file ((path string))
  (probe-delectus-file (pathname path)))
