;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; data-engine
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; the package for the delectus data engine

(defpackage #:data-engine
  (:nicknames :data)
  (:use #:cl #:sqlite #:fare-csv))



