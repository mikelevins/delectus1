;;;; ***********************************************************************
;;;;
;;;; Name:          database.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       database interface
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; ABOUT
;;;---------------------------------------------------------------------
;;; implements a database API that hides the details of the file format

(in-package :data)

;;; =====================================================================
;;; database interface
;;; =====================================================================


;;;---------------------------------------------------------------------
;;; CLASS database
;;;---------------------------------------------------------------------
;;; the common type of database representations

(defclass database ()())

;;;---------------------------------------------------------------------
;;; database API
;;;---------------------------------------------------------------------

(defgeneric valid? (db))
(defgeneric table-names (db))
(defgeneric table-columns (db table-name))
(defgeneric table-row-count (db table-name))
(defgeneric table-rows (db table-name &key from count))
(defgeneric table-row (db table-name index))
