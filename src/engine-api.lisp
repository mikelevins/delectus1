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
;;; creating the temporary "latest_items" table
;;; ---------------------------------------------------------------------

(defmethod db-check-latest-items-table-exists ((db sqlite-handle))
  (bind ((sql vals (sqlgen-check-latest-items-table-exists))
         (found-table (apply 'execute-to-list db sql vals)))
    (if found-table t nil)))

(defmethod db-create-latest-items-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-latest-items-table)))
    (apply 'execute-to-list db sql vals)))

;;; ---------------------------------------------------------------------
;;; counting the latest items
;;; ---------------------------------------------------------------------

(defmethod db-count-latest-items ((db sqlite-handle))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-count-latest-items)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-items ((dbpath pathname))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-count-latest-items db)))

;;; ---------------------------------------------------------------------
;;; fetching the latest items
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-items ((db sqlite-handle)
                                &key
                                  (offset 0)
                                  (limit *default-result-items-per-page*))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-get-latest-items :offset offset :limit limit)))
    (apply 'execute-to-list db sql vals)))

(defmethod get-latest-items ((dbpath pathname)
                             &key
                               (offset 0)
                               (limit *default-result-items-per-page*))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-get-latest-items db  :offset offset :limit limit)))


;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------

;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (setf $wordtest100-path (path "~/Desktop/wordtest100.delectus2"))
;;; (setf $wordtest1k-path (path "~/Desktop/wordtest1k.delectus2"))
;;; (setf $wordtest10k-path (path "~/Desktop/wordtest10k.delectus2"))
;;; (setf $wordtest100k-path (path "~/Desktop/wordtest100k.delectus2"))

;;; (time (count-latest-items $movies-test-path))
;;; (time (count-latest-items $zips-test-path))
;;; (time (count-latest-items $wordtest100-path))
;;; (time (count-latest-items $wordtest1k-path))
;;; (time (count-latest-items $wordtest10k-path))
;;; (time (count-latest-items $wordtest100k-path))

;;; (time (get-latest-items $movies-test-path))
;;; (time (get-latest-items $zips-test-path))
;;; (time (get-latest-items $wordtest100-path))
;;; (time (get-latest-items $wordtest10k-path))
;;; (time (get-latest-items $wordtest100k-path))
