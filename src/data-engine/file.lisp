;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       managing database files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :data)

;;;---------------------------------------------------------------------
;;; CLASS sqlite-file
;;;---------------------------------------------------------------------

(defclass sqlite-file ()
  ((path :accessor path :initform nil :initarg :path)))

(defmethod sqlite-file ((path pathname))
  (make-instance 'sqlite-file :path path))

(defmethod sqlite-file ((path string))
  (sqlite-file (pathname path)))

;;; (defparameter $sqlite-file (sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2"))

(defmethod sqlite-file-valid? ((file sqlite-file))
  (ensure-valid-sqlite-file (path file)))

(defmethod sqlite-file-table-names ((file sqlite-file))
  (if (sqlite-file-valid? file)
      (sqlite-list-tables (path file))
      (error "Not a SQLite file: ~S" (path file))))

;;; (sqlite-file-valid? $sqlite-file)
;;; (sqlite-file-table-names $sqlite-file)

(defmethod sqlite-file-table-columns ((file sqlite-file)(table-name string))
  (if (sqlite-file-valid? file)
      (sqlite-list-table-columns (path file) table-name)
      (error "Not a SQLite file: ~S" (path file))))

;;; (sqlite-file-table-columns $sqlite-file "contents")

(defmethod sqlite-file-table-column-names ((file sqlite-file)(table-name string))
  (mapcar #'second (sqlite-file-table-columns file table-name)))

;;; (sqlite-file-table-column-names $sqlite-file "contents")

(defmethod sqlite-file-table-rows ((file sqlite-file)(table-name string)
                                   &key (from 0)(count nil))
  (if (sqlite-file-valid? file)
      (sqlite-get-table-rows (path file) table-name :from from :count count)
      (error "Not a SQLite file: ~S" (path file))))

;; (sqlite-file-table-rows $sqlite-file "contents" :from 0 :count 5)
;; (sqlite-file-table-rows $sqlite-file "contents" :from 100 :count 5)


(defmethod sqlite-file-table-row ((file sqlite-file)(table-name string) index)
  (if (sqlite-file-valid? file)
      (sqlite-get-table-row (path file) table-name index)
      (error "Not a SQLite file: ~S" (path file))))

;; (sqlite-file-table-row $sqlite-file "contents" 0)
;; (sqlite-file-table-row $sqlite-file "contents" 100)