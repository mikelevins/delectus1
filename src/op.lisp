;;;; ***********************************************************************
;;;;
;;;; Name:          op.lisp
;;;; Project:       delectus 2
;;;; Purpose:       constructing ops
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; op format:
;;;
;;; (optype opid origin revision itemid deleted name columns)
;;;
;;; in lisp forms, the columns are collected into a list parameter
;;; in database tables, each element of columns gets a database column

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numebrs autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

(defun json-val (x) (or x :false))

(defun make-column (label &key (id (new-identifier)) (deleted nil) (order *minimum-column-order*) (type "text") (sort nil))
  (assert (stringp label)() "You must supply a label for the column")
  `("id" ,(json-val id)
         "label" ,(json-val label)
         "deleted" ,(json-val deleted)
         "order" ,(json-val order)
         "type" ,(json-val type)
         "sort"  ,(json-val sort)))



