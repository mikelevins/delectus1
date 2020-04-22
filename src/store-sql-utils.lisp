;;;; ***********************************************************************
;;;;
;;;; Name:          store-sql-utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       helper functions supporting store-sqlgen
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :sqlgen)
(in-readtable :interpol-syntax)

;;; =====================================================================
;;; helper functions
;;; =====================================================================

(defun make-metadata-params (optype opid origin revision timestamp item name deleted peer)
  (values (list "optype" "opid" "origin" "revision" "timestamp" "item" "name" "deleted" "peer")
          (list optype opid origin revision timestamp item name deleted peer)))

(defun make-column-params (column-data)
  (values (mapcar (lambda (c)(fset:@ c :|id|)) column-data)
          (mapcar #'delectus::to-json column-data)))

(defun make-item-params (column-data vals)
  (assert (= (length column-data)(length vals))()
          "The number of values must equal the number of columns")
  (values (mapcar (lambda (c)(fset:@ c :|id|)) column-data)
          vals))

;;; to help mark out SQL code visually below
(defun SQL (s)(delectus::trim s))
