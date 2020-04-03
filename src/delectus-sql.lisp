;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific sql constructors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sql)

;;; ---------------------------------------------------------------------
;;; templates
;;; ---------------------------------------------------------------------

(defparameter +create-delectus-table-template+
  "
CREATE TABLE delectus (
  id TEXT,
  origin TEXT,
  format TEXT,
  next_revision TEXT,
);
")

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(register-emb "create-delectus-table" +create-delectus-table-template+)

(defun create-delectus-table ()
  (execute-emb "create-delectus-table"))

;;; (create-delectus-table)
