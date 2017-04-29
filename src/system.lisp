;;;; ***********************************************************************
;;;;
;;;; Name:          system.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       common system parameters and functions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.system)

;;; +system-rowid-label+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; column label reserved for row ids

(defparameter +system-rowid-label+ "rowid")

;;; +reserved-column-labels+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; reserved column labels unavailable for user columns

(defparameter +reserved-column-labels+ (list +system-rowid-label+ "oid" "_rowid_"))

;;; valid-column-label? (thing)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns true if and only if THING is a string that can be used as a
;;; column label.

(defmethod valid-column-label? (thing) nil)

(defmethod valid-column-label? ((thing string))
  (not (member thing +reserved-column-labels+ :test #'equalp)))

