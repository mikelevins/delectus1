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

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
(defclass store ()
  (;; a reference to the database in which data are stored
   (db-path :reader db-path :initform nil :initarg :db-path)))

(defmethod create-delectus-file ((path pathname))
  (with-open-database (db path)
    (with-transaction db
      (execute-non-query db "create table delectus (format_version integer)")
      (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
      (execute-non-query db "create table contents (rowid integer primary key, deleted boolean)")
      (execute-non-query db "create table column_order (column_name string)")
      (execute-non-query db "create table deleted_columns (column_name string)")))
  path)

(defmethod create-delectus-file ((path string))
  (create-delectus-file (pathname path)))

(defmethod valid-delectus-file? ((path pathname))
  (and (probe-file path)
       (uiop/pathname:file-pathname-p path)
       (handler-case (with-open-database (db path)
                       (execute-non-query db "pragma schema_version")
                       t)
         ;; the file was not a SQLite database
         (sqlite-error (err) (warn "Not a Delectus store file (not a database): ~S" path) nil)
         ;; some other error occurred when reading the file
         (error (err) (warn "Unable to open file: ~S" path) nil))
       ;; if we reach this code then the path names a SQLite database file
       ;; check to see whether it has the required Delectus tables:
       ;; 1. table: delectus
       (handler-case (with-open-database (db path)
                       (execute-non-query db "select * from delectus")
                       t)
         (sqlite-error (err) (warn "Not a Delectus store file (missing the Delectus table): ~S" path) nil))
       ;; 2. table: contents
       (handler-case (with-open-database (db path)
                       (execute-non-query db "select * from contents limit 1")
                       t)
         (sqlite-error (err) (warn "Not a Delectus store file (missing the Contents table): ~S" path) nil))
       ;; 3. table: column_order
       (handler-case (with-open-database (db path)
                       (execute-non-query db "select * from column_order limit 1")
                       t)
         (sqlite-error (err) (warn "Not a Delectus store file (missing the column_order table): ~S" path) nil))))

(defmethod valid-delectus-file? ((path string))
  (valid-delectus-file? (pathname path)))

(defmethod initialize-instance :after ((store store) &rest initargs &key &allow-other-keys)
  (let ((store-path (db-path store)))
    (if (probe-file store-path)
        (unless (valid-delectus-file? store-path)
          (setf (slot-value store 'db-path) nil)
          (error "File is not a valid Delectus store: ~A" store-path))
      (create-delectus-file store-path))))

(defmethod contents-columns ((path pathname))
  (with-open-database (db path)
    (execute-to-list db "pragma table_info(contents)")))

(defmethod contents-columns ((path string))
  (contents-columns (pathname path)))