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
(defparameter *editlog-table-name* "editlog")
;; (defparameter *listnames-table-name* "listnames")
;; (defparameter *comments-table-name* "comments")
;; (defparameter *columns-table-name* "columns")
;; (defparameter *items-table-name* "items")

;;; column names
;;; ------------

(defparameter *delectus-columns* [ :|listid| :|format| :|created| :|modified| :|next_revision| :|next_itemid|])
(defparameter *editlog-columns* [ :|target| :|origin| :|revision| :|order| :|timestamp| :|data| ])
;; (defparameter *listnames-metadata-columns* [:|revision| :|origin| :|timestamp| :|name|])
;; (defparameter *comments-metadata-columns* [:|revision| :|origin| :|timestamp| :|comment|])
;; (defparameter *columns-metadata-columns* [:|revision| :|origin| :|timestamp|])
;; (defparameter *items-metadata-columns* [:|revision| :|origin| :|timestamp| :|itemid| :|deleted|])

;;; column parameters
;;; -----------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* (coerce 100.0 'double-float))
(defparameter *minimum-column-order* (coerce 100.0 'double-float))
(defparameter *default-initial-column-order* *minimum-column-order*)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; op parameters
;;; -----------------

(defparameter *minimum-op-order* (coerce 100.0 'double-float))
(defparameter *op-order-interval* (coerce 100.0 'double-float))
