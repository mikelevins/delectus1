;;;; ***********************************************************************
;;;;
;;;; Name:          utils-sqlite.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       utilities for operating on sqlite files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :data)

;;; GENERIC FUNCTION ensure-valid-sqlite-file (path)
;;; ---------------------------------------------------------------------
;;; returns PATH if it's a valid SQLite file; returns NIL if it isn't

(defmethod ensure-valid-sqlite-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           ;; the right way to check whether a file is a SQLite file,
           ;; according to SQLite docs:
           (execute-non-query db "pragma schema_version"))
         path)))

(defmethod ensure-valid-sqlite-file ((path string))
  (ensure-valid-sqlite-file (pathname path)))

;;; tests:
;;; should return the pathname because it's a valid sqlite file:
;;; (ensure-valid-sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; should return NIL because it isn't:
;;; (ensure-valid-sqlite-file "/Users/mikel/.emacs")
;;; should return NIL because it doesn't exist:
;;; (ensure-valid-sqlite-file "/Users/brobdingnag/.emacs")

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
