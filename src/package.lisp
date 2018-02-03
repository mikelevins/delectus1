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

;;; ---------------------------------------------------------------------
;;; delectus.version
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Definitions of delectus application and file-format versions and
;;; utilities for retrieving them.

(defpackage #:delectus.version
  (:use #:cl)
  (:export
   #:+delectus-major-version+
   #:+delectus-minor-version+
   #:+delectus-patch-version+
   #:delectus-version
   #:delectus-version-string
   #:+delectus-format-major-version+
   #:+delectus-format-minor-version+
   #:delectus-format-version-string))


