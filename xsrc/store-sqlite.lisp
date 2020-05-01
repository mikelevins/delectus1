;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlite.lisp
;;;; Project:       delectus 2
;;;; Purpose:       basic operations on sqlite files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; check the SQLite library version

(fli:define-foreign-function 
    (sqlite3-libversion "sqlite3_libversion" :source)
    ()
  :result-type (:pointer :char)
  :language :ansi-c)

;;; (fli:convert-from-foreign-string (sqlite3-libversion))
;;; currently reports 3.28.0 for the SQLite included with macOS Catalina
;;; (mapcar #'cffi:foreign-library-pathname (cffi:list-foreign-libraries :loaded-only t))


;;; GENERIC FUNCTION sqlite-compile-options (path)
;;; ---------------------------------------------------------------------
;;; check the SQLite library's compile-time options

(defmethod sqlite-compile-options ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (file-pathname-p path)
         (handler-case (with-open-database (db path)
                         ;; the right way to check whether a file is a SQLite file,
                         ;; according to SQLite docs:
                         (execute-to-list db "pragma compile_options"))
           (condition (c)
             (declare (ignore c))
             nil)))))

(defmethod sqlite-compile-options ((path string))
  (sqlite-compile-options (pathname path)))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (sqlite-compile-options $zippath)

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
           (condition (c)
             (declare (ignore c))
             nil))
         path)))

(defmethod valid-sqlite-file? ((path string))
  (valid-sqlite-file? (pathname path)))

;;; tests:
;;; should return the pathname because it's a valid sqlite file:
;;; (valid-sqlite-file? "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3")
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

;;; (sqlite-list-tables "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3")

;;; GENERIC FUNCTION sqlite-list-table-column-info (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a list of column descriptions from the named table in the
;;; file at PATH

(defmethod db-sqlite-table-column-info ((db sqlite-handle) (table-name string))
  (mapcar (lambda (info)(apply #'column-info info))
          (sqlite:execute-to-list db (format nil "pragma table_info(~S)" table-name))))

(defmethod sqlite-table-column-info ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (db-sqlite-table-column-info db table-name)))

(defmethod sqlite-table-column-info ((path string) (table-name string))
  (sqlite-table-column-info (pathname path) table-name))

;;; (sqlite-table-column-info "/Users/mikel/Desktop/testlist.delectus2" "listdata")

;;; GENERIC FUNCTION sqlite-list-table-column-names (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a list of column names from the named table in the file at
;;; PATH

(defmethod sqlite-list-table-column-names ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (mapcar #'cadr (sqlite:execute-to-list db (format nil "pragma table_info(~S)" table-name)))))

(defmethod sqlite-list-table-column-names ((path string) (table-name string))
  (sqlite-list-table-column-names (pathname path) table-name))

;;; (sqlite-list-table-column-names "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data")

;;; GENERIC FUNCTION sqlite-count-table-rows (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a count of rows in the named table in the file at PATH

(defmethod sqlite-count-table-rows ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (sqlite:execute-single db (format nil "select count(*) from ~A" table-name))))

(defmethod sqlite-count-table-rows ((path string) (table-name string))
  (sqlite-count-table-rows (pathname path) table-name))

;;; (sqlite-count-table-rows "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data")

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

;;; (sqlite-get-table-rows "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data" :from 0 :count 10)
;;; (sqlite-get-table-rows "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data" :from 100 :count 10)

;;; GENERIC FUNCTION sqlite-get-table-row (path table-name index))
;;; ---------------------------------------------------------------------
;;; returns the row at INDEX from the table named TABLE-NAME in the
;;; file at PATH.

(defmethod sqlite-get-table-row ((path pathname) (table-name string) (index integer))
  (sqlite:with-open-database (db path)
    (sqlite:execute-to-list db (format nil "select * from ~a limit ~d,~d" table-name index 1))))

(defmethod sqlite-get-table-row ((path string) (table-name string) (index integer))
  (sqlite-get-table-row (pathname path) table-name index))

;;; (sqlite-get-table-row "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data" 0)
;;; (sqlite-get-table-row "/Users/mikel/Workshop/data/kinder/kinder_data.sqlite3" "kinder_data" 100)

