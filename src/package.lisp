;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; delectus
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; the top-level package for the delectus application

(defpackage #:delectus
  (:use #:cl #:sqlite #:fare-csv #+capi #:capi))



