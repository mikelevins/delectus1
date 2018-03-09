;;;; ***********************************************************************
;;;;
;;;; Name:          database.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       managing databases
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; ABOUT
;;;---------------------------------------------------------------------
;;; implements a database API that hides the details of the file format
;;; the only database class presently supported is sqlite-file

(in-package :data)


;;;---------------------------------------------------------------------
;;; CLASS sqlite-file
;;;---------------------------------------------------------------------
;;; a database represented as a SQLite file

(defclass sqlite-file ()
  ((path :accessor path :initform nil :initarg :path)))

;;; GENERIC FUNCTION sqlite-file (path)
;;;---------------------------------------------------------------------
;;; returns a newly-created sqlite-file whose contents are the file at
;;; PATH.

(defmethod sqlite-file ((path pathname))
  (make-instance 'sqlite-file :path path))

(defmethod sqlite-file ((path string))
  (sqlite-file (pathname path)))

;;; (defparameter $sqlite-file (sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2"))

;;; GENERIC FUNCTION valid? (db)
;;;---------------------------------------------------------------------
;;; returns a true value if the database is valid, NIL otherwise
;;; a valid sqlite-file database is an existing SQLite file that
;;; we can read from

(defmethod valid? ((db sqlite-file))
  (valid-sqlite-file? (path db)))

;;; GENERIC FUNCTION table-names (db)
;;;---------------------------------------------------------------------
;;; returns a list of table names from the database

(defmethod table-names ((db sqlite-file))
  (if (valid? db)
      (sqlite-list-tables (path db))
      (error "Invalid database: ~S" db)))

;;; (table-names $sqlite-file)

;;; GENERIC FUNCTION table-columns (db table-name)
;;;---------------------------------------------------------------------
;;; returns a list of column info from the table named TABLE-NAME, or
;;; NIL if there is no such table.

(defmethod table-columns ((db sqlite-file)(table-name string))
  (if (valid? db)
      (sqlite-list-table-columns (path db) table-name)
      (error "Invalid database: ~S" db)))

;;; (table-columns $sqlite-file "contents")

;;; GENERIC FUNCTION table-column-names (db table-name)
;;;---------------------------------------------------------------------
;;; returns a list of column names from the table named TABLE-NAME, or
;;; NIL if there is no such table.

(defmethod table-column-names ((db sqlite-file)(table-name string))
  (mapcar #'second (table-columns db table-name)))

;;; (table-column-names $sqlite-file "contents")

;;; GENERIC FUNCTION table-row-count (db table-name)
;;;---------------------------------------------------------------------
;;; returns a count of rows from the named table in the file at
;;; PATH.

(defmethod table-row-count ((db sqlite-file)(table-name string))
  (if (valid? db)
      (sqlite-count-table-rows (path db) table-name)
      (error "Invalid database: ~S" db)))

;; (table-row-count $sqlite-file "contents")

;;; GENERIC FUNCTION table-rows (db table-name)
;;;---------------------------------------------------------------------
;;; returns a list of rows from the named table in the file at
;;; PATH. Collects COUNT rows starting at index FROM. If COUNT is NIL,
;;; returns all rows.

(defmethod table-rows ((db sqlite-file)(table-name string)
                                   &key (from 0)(count nil))
  (if (valid? db)
      (sqlite-get-table-rows (path db) table-name :from from :count count)
      (error "Invalid database: ~S" db)))

;; (table-rows $sqlite-file "contents" :from 0 :count 5)
;; (table-rows $sqlite-file "contents" :from 100 :count 5)

;;; GENERIC FUNCTION table-row (db table-name index)
;;;---------------------------------------------------------------------
;;; returns the row at INDEX from the table named TABLE-NAME in the
;;; database DB

(defmethod table-row ((db sqlite-file)(table-name string) index)
  (if (valid? db)
      (sqlite-get-table-row (path db) table-name index)
      (error "Invalid database: ~S" db)))

;; (table-row $sqlite-file "contents" 0)
;; (table-row $sqlite-file "contents" 100)
