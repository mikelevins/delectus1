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

#+lispworks
(fli:define-foreign-function 
    (fli-sqlite3-libversion "sqlite3_libversion" :source)
    ()
  :result-type (:pointer :char)
  :language :ansi-c)

;;; (fli:convert-from-foreign-string (delectus::fli-sqlite3-libversion))
;;; currently reports 3.28.0 for the SQLite included with macOS Catalina
;;; (mapcar #'cffi:foreign-library-pathname (cffi:list-foreign-libraries :loaded-only t))


;;; GENERIC FUNCTION sqlite-library-version (path)
;;; ---------------------------------------------------------------------
;;; check the SQLite library's version. 

(defun sqlite-library-version ()
  (let* ((tmp-name (concatenate 'string
                                (symbol-name (gensym "temp"))
                                ".db"))
         (tmp-path (merge-pathnames tmp-name "/tmp/")))
    (with-open-database (db tmp-path)
      (execute-single db "SELECT sqlite_version()"))))

;;; (sqlite-library-version "/tmp/tmp.db")

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

;;; (defparameter $testpath (uiop:native-namestring "~/.emacs"))
;;; (sqlite-compile-options $testpath)

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
;;; (defparameter $testpath (uiop:native-namestring "~/.emacs"))
;;; (valid-sqlite-file? $testpath)
;;; (defparameter $moviespath (uiop:native-namestring "~/Desktop/Movies-test.delectus2"))
;;; (valid-sqlite-file? $moviespath)

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

;;; (sqlite-table-column-info "/Users/mikel/Desktop/testlist.delectus2" "items")

