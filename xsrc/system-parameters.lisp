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

(defparameter *default-result-items-per-page* 50)

;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *listnames-table-name* "listnames")
(defparameter *comments-table-name* "comments")
(defparameter *columns-table-name* "columns")
(defparameter *items-table-name* "items")

;;; column names
;;; ------------

(defparameter *delectus-columns* [ :|listid| :|format| :|created| :|modified| ])
(defparameter *listnames-op-columns* [ :|origin| :|revision| :|timestamp| :|name| ])
(defparameter *comments-op-columns* [ :|origin| :|revision| :|timestamp| :|comment| ])
(defparameter *columns-op-columns* [ :|origin| :|revision| :|timestamp| ])
(defparameter *items-op-columns* [ :|origin| :|revision| :|item-order| :|timestamp| :|itemid| :|deleted| ])

;;; column parameters
;;; -----------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* (coerce 100.0 'double-float))
(defparameter *minimum-column-order* (coerce 100.0 'double-float))
(defparameter *default-initial-column-order* *minimum-column-order*)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; op parameters
;;; -----------------

(defparameter *minimum-item-order* (coerce 100.0 'double-float))
(defparameter *item-order-interval* (coerce 100.0 'double-float))
(defparameter *minimum-item-order* (coerce 100.0 'double-float))
(defparameter *item-order-interval* (coerce 100.0 'double-float))
