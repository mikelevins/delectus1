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
      (execute-non-query db "create table contents (deleted boolean)")
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

(defmethod ensure-delectus-2-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           (execute-non-query db "pragma schema_version")
           (execute-non-query db "select * from delectus"))
         path)))

(defmethod ensure-delectus-2-file ((path string))
  (ensure-delectus-2-file (pathname path)))

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

;;; (column-order "/Users/mikel/Desktop/junior-movies.delectus2")

;;; deleted-columns ((path pathname))
;;; ---------------------------------------------------------------------
;;; returns the labels of columnsmarked deleted
(defmethod deleted-columns ((path pathname))
  (with-open-database (db path)
    ;; query returns matching rows as a list of lists
    (let ((rows (execute-to-list db "select column_name from deleted_columns")))
      ;; return the single result from each row
      (mapcar #'car rows))))

(defmethod deleted-columns ((path string))
  (deleted-columns (pathname path)))

;;; (deleted-columns "/Users/mikel/Desktop/junior-movies.delectus2")

;;; add-column (path label)
;;; ---------------------------------------------------------------------
(defmethod add-column ((path pathname)(label string))
  (with-open-database (db path)
    (handler-case (with-open-database (db path)
                    (let ((add-sql (format nil "ALTER TABLE \"contents\" ADD COLUMN ~S" label))
                          (delete-sql "DELETE FROM \"column_order\""))
                      (execute-non-query db add-sql)
                      ;; now update the column order to include the new column
                      ;; TODO: this implementation discards the old column order and
                      ;; uses the actual order of columns from the contents table
                      ;; in order to enable column_order to record user preferences
                      ;; this must be changed to preserve the old order and restore it
                      ;; with the new column added to the end
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

;;; ---------------------------------------------------------------------
;;; db accessors
;;; ---------------------------------------------------------------------

(defun visible-delectus-columns (document-path)
  (remove "deleted" (contents-labels document-path)
          :test #'equal))

;;; visible-delectus-columns ((path pathname))
;;; ---------------------------------------------------------------------
;;; returns the columns that are not deleted, excluding the "deleted" column
(defun visible-delectus-columns (document-path)
  (let ((deleta (cons "deleted" (deleted-columns document-path)))
        (cols (column-order document-path)))
    (loop for del in deleta do (setf cols (remove del cols :test #'string=)))
    cols))

;;; (visible-delectus-columns "/Users/mikel/Desktop/Movies.delectus2")
;;; (visible-delectus-columns "/Users/mikel/Desktop/junior-movies.delectus2")

(defun visible-delectus-rows (document-path)
  (let* ((column-labels (visible-delectus-columns document-path))
         (select-sql (format nil "select ~{\"~A\"~^, ~} from contents where deleted = 0" column-labels)))
    (with-open-database (db document-path)
      (execute-to-list db select-sql))))

(defun count-all-rows (document-path)
  (with-open-database (db document-path)
    (first (first (execute-to-list db "select Count(*) from contents")))))

;;; (count-all-rows "/Users/mikel/Desktop/zipcode_20k.delectus2")
;;; (count-all-rows "/Users/mikel/Desktop/Movies.delectus2")
;;; (count-all-rows "/Users/mikel/Desktop/junior-movies.delectus2")

;;; filters out deleted rows
(defun count-visible-rows (document-path)
  (with-open-database (db document-path)
    (first (first (execute-to-list db "select Count(*) from contents where deleted = 0")))))

;;; (count-visible-rows "/Users/mikel/Desktop/zipcode_20k.delectus2")
;;; (count-visible-rows "/Users/mikel/Desktop/Movies.delectus2")
;;; (count-visible-rows "/Users/mikel/Desktop/junior-movies.delectus2")
