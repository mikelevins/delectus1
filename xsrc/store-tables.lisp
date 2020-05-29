;;;; ***********************************************************************
;;;;
;;;; Name:          store-tables.lisp
;;;; Project:       delectus 2
;;;; Purpose:       working with the list file's tables
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; the 'delectus' table
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating it 
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle) (listid string)(format string))
  (assert (identity-string? listid)()
          "Expected an identity-string for the :LISTID paramter, but found ~S"
          listid)
  (bind ((create-sql create-vals (sqlgen-create-delectus-table))
         (init-sql init-vals (sqlgen-init-delectus-table listid :format format)))
    (apply 'execute-non-query db create-sql create-vals)
    (apply 'execute-non-query db init-sql init-vals)))

;;; =====================================================================
;;; the 'editlog' table
;;; =====================================================================

(defmethod db-create-editlog-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-editlog-table)))
    (apply 'execute-non-query db create-sql create-vals)))

