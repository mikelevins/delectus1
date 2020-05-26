;;;; ***********************************************************************
;;;;
;;;; Name:          system-parameters.lisp
;;;; Project:       delectus 2
;;;; Purpose:       set up pathnames and other parameters
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; root of the Delectus app's file tree
;;; ---------------------------------------------------------------------

(defparameter *delectus-root-pathname* (asdf:system-relative-pathname :delectus ""))

;;; query parameters
;;; ---------------------------------------------------------------------

(defparameter *default-result-items-per-page* 25)

;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *editlog-table-name* "listnames")

;;; column names
;;; ------------

(defparameter *delectus-columns* [ :|listid| :|format| :|created| :|modified| :|next_revision| :|next_itemid|])
(defparameter *editlog-columns* [ :|target| :|origin| :|revision| :|order| :|timestamp| :|data|])


;;; column parameters
;;; -----------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 100.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 100.0)
(defparameter *default-initial-column-order* *minimum-column-order*)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

