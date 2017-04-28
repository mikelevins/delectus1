;;;; ***********************************************************************
;;;;
;;;; Name:          system.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       common system parameters and functions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; system column labels
;;; ---------------------------------------------------------------------
;;; columns that every delectus contents table possesses, regardless
;;; of what columns a user supplies.

(defparameter +system-rowid-label+ "rowid")

(defparameter +reserved-column-labels (list +system-rowid-label+ "oid" "_rowid_"))

(defmethod valid-column-label? (thing) nil)

(defmethod valid-column-label? ((thing string))
  (not (member thing +reserved-column-labels :test #'equalp)))

