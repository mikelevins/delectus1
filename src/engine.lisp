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

(defmethod make-delectus-file ((path pathname))
  (let ((file-type (pathname-type path)))
    (assert (equalp "delectus2" file-type)()
      "The type extension of the Delectus file must be \"delectus2\"; found ~S" file-type)
    (with-open-database (db path)
      (with-transaction db
        (execute-non-query db "create table delectus (format_version integer)")
        (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
        (execute-non-query db "create table contents (rowid integer primary key) without rowid")))
    path))

(defmethod make-delectus-file ((path string))
  (make-delectus-file (pathname path)))

(defmethod delectus-file-version ((path pathname))
  (with-open-database (db path)
    (with-transaction db
      (let ((table-name (execute-single db "SELECT name FROM sqlite_master WHERE type='table' AND name='delectus'")))
        (if table-name
            (let ((version-numbers (execute-to-list db "SELECT * FROM delectus")))
              (if version-numbers
                  (first version-numbers)
                nil))
          nil)))))

(defmethod delectus-file-version ((path string))
  (delectus-file-version (pathname path)))

(defmethod add-column ((path pathname)(label string))
  (with-open-database (db path)
    (with-transaction db
      (let ((sql (format nil "ALTER TABLE contents ADD COLUMN ~A string" label)))
        (execute-non-query db sql))
      label)))

(defmethod add-column ((path string)(label string))
  (add-column (pathname path) label))

(defmethod all-rows ((path pathname))
  (with-open-database (db path)
    (with-transaction db
      (execute-to-list db "select * from contents"))))

(defmethod all-rows ((path string))
  (all-rows (pathname path)))

;;; (make-delectus-file "/Users/mikel/Desktop/tmp.delectus2")
;;; (delectus-file-version "/Users/mikel/Desktop/tmp.delectus2")
;;; (add-column "/Users/mikel/Desktop/tmp.delectus2" "Name")
;;; (all-rows "/Users/mikel/Desktop/tmp.delectus2")
