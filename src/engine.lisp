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
      (execute-non-query db "insert into column_order (column_name) values (?)" "rowid")
      (execute-non-query db "insert into column_order (column_name) values (?)" "deleted")
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


;;; contents-columns ((path pathname))
;;; ---------------------------------------------------------------------
;;; returns a list of column descriptions. each column description has
;;; the following format:
;;; (column-index column-name column-type notnull default-value primary-key?)
;;; the fields of the description are as follows:
;;; - field        type        notes
;;; ----------------------------------------------
;;; - column-index  integer    order added, indexed from 0
;;; - column-name   string     string label
;;; - column-type   string     string SQLite type-name
;;; - notnull       0|1        1 if NULL values are allowed
;;; - default-value string|NIL either a string value or NIL
;;; - primary-key?  0|1        1 if the column is used in computing the primary key

(defmethod contents-columns ((path pathname))
  (with-open-database (db path)
    (execute-to-list db "pragma table_info(contents)")))

(defmethod contents-columns ((path string))
  (contents-columns (pathname path)))

(defmethod contents-labels ((path pathname))
  (let ((column-descriptions (contents-columns path)))
    (mapcar #'second
            column-descriptions)))

(defmethod contents-labels ((path string))
  (contents-labels (pathname path)))

;;; column-order ((path pathname))
;;; ---------------------------------------------------------------------
;;; returns the current column order of the Delectus store.  the
;;; column order is a list of column labels in the order they should
;;; appear in results when retrieving the contents of the store. In
;;; other words, a SELECT that retrieves rows from the "contents"
;;; table should request the columns in
;;; the same order as the labels in column-order

(defmethod column-order ((path pathname))
  (with-open-database (db path)
    ;; query returns matching rows as a list of lists
    (let ((rows (execute-to-list db "select column_name from column_order")))
      ;; return the single result from each row
      (mapcar #'car rows))))

(defmethod column-order ((path string))
  (column-order (pathname path)))

;;; add-column (path label)
;;; ---------------------------------------------------------------------

(defmethod add-column ((path pathname)(label string))
  (with-open-database (db path)
    (handler-case (with-open-database (db path)
                    (let ((add-sql (format nil "ALTER TABLE \"contents\" ADD COLUMN ~S" label))
                          (delete-sql "DELETE FROM \"column_order\""))
                      (execute-non-query db add-sql)
                      (execute-non-query db delete-sql)
                      (let ((contents-labels (let ((column-descriptions (execute-to-list db "pragma table_info(contents)")))
                                               (mapcar #'second
                                                       column-descriptions))))
                        (dolist (lbl contents-labels)
                          (execute-non-query db "insert into column_order (column_name) values (?)" lbl))))
                    t)
      (sqlite-error (err) (warn "Column already exists: ~S" label) nil))))

(defmethod add-column ((path string)(label string))
  (add-column (pathname path) label))