;;;; ***********************************************************************
;;;;
;;;; Name:          store-listnames-table.lisp
;;;; Project:       delectus 2
;;;; Purpose:       creating the listnames table
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; creating the 'listnames' table
;;; ---------------------------------------------------------------------

(defmethod db-create-listnames-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-listnames-table)))
    (apply 'execute-non-query db create-sql create-vals)))

