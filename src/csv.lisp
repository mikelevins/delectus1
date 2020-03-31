;;;; ***********************************************************************
;;;;
;;;; Name:          csv.lisp
;;;; Project:       delectus 2
;;;; Purpose:       I/O operations on csv files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defun read-csv (path)
  (fare-csv:with-rfc4180-csv-syntax ()
    (let* ((fare-csv:*separator* #\,))
      (fare-csv:read-csv-file path))))

;;; (defparameter $csvpath "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (time (defparameter $data (read-csv $csvpath)))
;;; (length $data)
;;; (elt $data 0)
;;; (elt $data 43191)

