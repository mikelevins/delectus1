;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       generating SQL for Delectus store operations
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :sqlgen)
(in-readtable :interpol-syntax)

;;; =====================================================================
;;; SQL constructors
;;; =====================================================================
;;; each sqlgen::constructor function returns two values: a SQL string, and
;;; a list of values to be bound to any '?' placeholders in the SQL
;;; string. If there are no placeholder variables in the SQL, then
;;; the second returned value is NIL.
;;;
;;; Each function definition is structured like this:
;;; (defun sqlgen::FUNCTION-NAME (arg0 arg1 ... argN)
;;;   (let (BINDING* ...)
;;;     (values
;;;       (SQL #?| ... some SQL code ... |)
;;;       (value-expression* ...))))
;;;
;;; The LET bindings are optional
;;; The SQL expression contains a CL-INTERPOL string
;;; that gives literal SQL code, optionally with
;;; interpolation expressions referring to variables
;;; from the enclosing environment.


;;; ---------------------------------------------------------------------
;;; sqlgen::create-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen::create-delectus-table ()
  (values
   (SQL #?|

CREATE TABLE `delectus` ( 
  `id` TEXT, 
  `origin` TEXT, 
  `format` TEXT, 
  `next_revision` INTEGER,
  `next_iref` INTEGER )

|)
   nil))


;;; (sqlgen::create-delectus-table)

;;; ---------------------------------------------------------------------
;;; sqlgen::create-identities-table
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sqlgen::create-listdata-table
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sqlgen::create-item-revision-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen::create-item-revision-origin-index ()
  (values
   (SQL #?|

CREATE INDEX `idx_item_revision_origin` 
ON `list_data` (`item`, `revision`, `origin`)

|)
   nil))

;;; (sqlgen::create-item-revision-origin-index)
