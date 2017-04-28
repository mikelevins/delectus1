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

;;; delectus
;;; ---------------------------------------------------------------------
;;; the top-level package for the delectus application

(defpackage #:delectus
  (:use #:cl #:sqlite #:fare-csv #+capi #:capi))

;;; delectus.version
;;; ---------------------------------------------------------------------
;;; definitions of delectus application and file-format versions and
;;; utilities for retrieving them.

(defpackage #:delectus.version
  (:use #:cl)
  (:export
   #:+delectus-major-version+
   #:+delectus-minor-version+
   #:+delectus-patch-version+
   #:+delectus-build-number+
   #:delectus-version
   #:delectus-version-string
   #:+delectus-format-version+      ; version number of the Delectus file format
   #:delectus-format-version-string ; version string for the Delectus file format
   ))

;;; delectus.system
;;; ---------------------------------------------------------------------
;;; common system parameters and related utilities

(defpackage #:delectus.system
  (:use #:cl)
  (:export 
   #:+system-rowid-label+    ; the system-reserved label for the private rowid column in all Delectus tables
   #:+reserved-column-labels ; column labels that are reserved to Delectus system code; these may not be used in user code
   #:valid-column-label?     ; a function that returns true if and only if a label is valid for use in a Delectus table
   ))

;;; delectus.utilities
;;; ---------------------------------------------------------------------
;;; general utilities

(defpackage #:delectus.utilities
  (:use #:cl)
  (:export 
   #:interpose
   ))

