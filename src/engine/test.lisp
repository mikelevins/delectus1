;;;; ***********************************************************************
;;;;
;;;; Name:          test.lisp
;;;; Project:       delectus 2
;;;; Purpose:       general tools for testing
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defmethod time-listfile-operations ((path pathname))
  (let ((db (time (connect path))))
    (unwind-protect
         (progn
           (time (db-create-latest-items-table db))
           (time (db-count-latest-items db))
           (time (db-get-latest-items db))
           (time (db-count-latest-filtered-items db :filter-text "aba"))
           (time (db-get-latest-filtered-items db :filter-text "aba")))
      (disconnect db))))

(defmethod time-listfile-operations ((path string))
  (time-listfile-operations (pathname path)))

;;; (time-listfile-operations (path "~/Desktop/wordtest100k.delectus2"))
