;;;; ***********************************************************************
;;;;
;;;; Name:          process-template.lisp
;;;; Project:       delectus 2
;;;; Purpose:       read sql templates into Lisp variables
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defmethod process-template ((path pathname) &optional env)
  (emb:execute-emb path :env env))

(defmethod process-template ((path string) &optional env)
  (process-template (pathname path) env))

;;; (process-template "/Users/mikel/Workshop/src/delectus/sql-templates/create-delectus-table.tmpl.sql")
;;; (process-template "/Users/mikel/Workshop/src/delectus/sql-templates/populate-delectus-table.tmpl.sql")
