;;;; ***********************************************************************
;;;;
;;;; Name:          engine-api.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a common API for support of UI components
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; getting items data
;;; ---------------------------------------------------------------------

(defmethod db-check-latest-items-table-exists ((db sqlite-handle))
  (bind ((sql vals (sqlgen-check-latest-items-table-exists))
         (found-table (apply 'execute-to-list db sql vals)))
    (if found-table t nil)))

(defmethod db-create-latest-items-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-latest-items-table)))
    (apply 'execute-to-list db sql vals)))

(defmethod db-count-latest-items ((db sqlite-handle))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-count-latest-items)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-items ((dbpath pathname))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-count-latest-items db)))

;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (time (count-latest-items $movies-test-path))

;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (time (count-latest-items $zips-test-path))

(defmethod db-get-latest-items ((db sqlite-handle))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-get-latest-items)))
    (apply 'execute-to-list db sql vals)))

(defmethod get-latest-items ((dbpath pathname))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-get-latest-items db)))

;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (time (get-latest-items $movies-test-path))

;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (time (get-latest-items $zips-test-path))
