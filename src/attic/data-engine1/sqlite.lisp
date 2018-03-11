;;;; ***********************************************************************
;;;;
;;;; Name:          sqlite.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       sqlite file interface
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; ABOUT
;;;---------------------------------------------------------------------
;;; implements the database API for sqlite files
;;; includes sqlite-specific utility functions

;;; =====================================================================
;;; sqlite utilities
;;; =====================================================================

(in-package :delectus.data)

;;; GENERIC FUNCTION valid-sqlite-file? (path)
;;; ---------------------------------------------------------------------
;;; returns PATH if it's a valid SQLite file; returns NIL if it isn't
;;; a valid sqlite-file database is an existing SQLite file that
;;; we can read from

(defmethod valid-sqlite-file? ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (file-pathname-p path)
         (handler-case (with-open-database (db path)
                         ;; the right way to check whether a file is a SQLite file,
                         ;; according to SQLite docs:
                         (execute-non-query db "pragma schema_version"))
           (condition (c) nil))
         path)))

(defmethod valid-sqlite-file? ((path string))
  (valid-sqlite-file? (pathname path)))

;;; tests:
;;; should return the pathname because it's a valid sqlite file:
;;; (valid-sqlite-file? "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; should return NIL because it isn't:
;;; (valid-sqlite-file? "/Users/mikel/.emacs")
;;; should return NIL because it doesn't exist:
;;; (valid-sqlite-file? "/Users/brobdingnag/.emacs")

;;; GENERIC FUNCTION sqlite-list-tables (path)
;;; ---------------------------------------------------------------------
;;; returns a list of table names from the file at PATH

(defmethod sqlite-list-tables ((path pathname))
  (sqlite:with-open-database (db path)
    (mapcar #'car (sqlite:execute-to-list db "SELECT name FROM sqlite_master WHERE type = \"table\""))))

(defmethod sqlite-list-tables ((path string))
  (sqlite-list-tables (pathname path)))

;;; (sqlite-list-tables "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")

;;; GENERIC FUNCTION sqlite-list-table-columns (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a list of column names from the named table in the file at
;;; PATH

(defmethod sqlite-list-table-columns ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (sqlite:execute-to-list db (format nil "pragma table_info(~S)" table-name))))

(defmethod sqlite-list-table-columns ((path string) (table-name string))
  (sqlite-list-table-columns (pathname path) table-name))

;;; (sqlite-list-table-columns "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents")
;;; returns NIL because there is no such table:
;;; (sqlite-list-table-columns "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "foo")

;;; GENERIC FUNCTION sqlite-count-table-rows (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a count of rows in the named table in the file at PATH

(defmethod sqlite-count-table-rows ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (sqlite:execute-single db (format nil "select count(*) from ~A" table-name))))

(defmethod sqlite-count-table-rows ((path string) (table-name string))
  (sqlite-count-table-rows (pathname path) table-name))

;;; (sqlite-count-table-rows "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents")

;;; GENERIC FUNCTION sqlite-get-table-rows (path table-name &key (from 0) (count nil)))
;;; ---------------------------------------------------------------------
;;; returns a list of rows from the named table in the file at
;;; PATH. Collects COUNT rows starting at index FROM. If COUNT is NIL,
;;; returns all rows.

(defmethod sqlite-get-table-rows ((path pathname) (table-name string) &key (from 0) (count nil))
  (sqlite:with-open-database (db path)
    (let ((count (or count
                     (sqlite-count-table-rows path table-name))))
      (sqlite:execute-to-list db (format nil "select * from ~a limit ~d,~d" table-name from count)))))

(defmethod sqlite-get-table-rows ((path string) (table-name string) &key (from 0) (count nil))
  (sqlite-get-table-rows (pathname path) table-name :from from :count count))

;;; (sqlite-get-table-rows "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents" :from 0 :count 10)
;;; (sqlite-get-table-rows "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents" :from 100 :count 10)

;;; GENERIC FUNCTION sqlite-get-table-row (path table-name index))
;;; ---------------------------------------------------------------------
;;; returns the row at INDEX from the table named TABLE-NAME in the
;;; file at PATH.

(defmethod sqlite-get-table-row ((path pathname) (table-name string) (index integer))
  (sqlite:with-open-database (db path)
    (sqlite:execute-to-list db (format nil "select * from ~a limit ~d,~d" table-name index 1))))

(defmethod sqlite-get-table-row ((path string) (table-name string) (index integer))
  (sqlite-get-table-row (pathname path) table-name index))

;;; (sqlite-get-table-row "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents" 0)
;;; (sqlite-get-table-row "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2" "contents" 100)

;;; =====================================================================
;;; database interface
;;; =====================================================================

;;;---------------------------------------------------------------------
;;; CLASS sqlite-file
;;;---------------------------------------------------------------------
;;; a database represented as a SQLite file

(defclass sqlite-file (database)
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
