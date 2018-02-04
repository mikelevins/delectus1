;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       managing database files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;;---------------------------------------------------------------------
;;; CLASS sqlite-file
;;;---------------------------------------------------------------------

(defclass sqlite-file ()
  ((path :accessor path :initform nil :initarg :path)))

(defmethod sqlite-file ((path pathname))
  (make-instance 'sqlite-file :path path))

(defmethod sqlite-file ((path string))
  (sqlite-file (pathname path)))

(defmethod sqlite-file-valid? ((file sqlite-file))
  (ensure-valid-sqlite-file (path file)))

(defmethod sqlite-file-tables ((file sqlite-file))
  (if (sqlite-file-valid? file)
      (sqlite-list-tables (path file))
      (error "Not a SQLite file: ~S" (path file))))

;;; (sqlite-file-valid? (sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2"))
;;; (sqlite-file-tables (sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2"))
