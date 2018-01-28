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

;;; ---------------------------------------------------------------------
;;; delectus.system
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Common system parameters and related utilities.

(defpackage #:delectus.system
  (:use #:cl)
  (:export 
   #:+system-rowid-label+
   #:+reserved-column-labels+
   #:valid-column-label?))

;;; ---------------------------------------------------------------------
;;; delectus.utilities
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; General utilities.

(defpackage #:delectus.utilities
  (:use #:cl)
  (:export 
   #:interpose
   ))

;;; ---------------------------------------------------------------------
;;; delectus.sqlite
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; sqlite utilities.

(defpackage #:delectus.sqlite
  (:use #:cl)
  (:export 
   ))

;;; ---------------------------------------------------------------------
;;; delectus.file
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Utilities for creating and populating Delectus 2 store files.

(defpackage #:delectus.file
  (:use #:cl #:sqlite #:fare-csv)
  (:export 
   #:create-delectus-file
   #:probe-delectus-file
   ))

;;; ---------------------------------------------------------------------
;;; delectus.legacy
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Utilities for converting files in old versions of Delectus formats
;;; to the current version.

(defpackage #:delectus.legacy
  (:use #:cl #:sqlite #:fare-csv)
  (:export 
   #:convert-delectus-sexp-file
   #:convert-delectus-csv-file
   ))

;;; ---------------------------------------------------------------------
;;; delectus.store
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Classes and functions for interacting with document data stored in
;;; Delectus files

(defpackage #:delectus.store
  (:use #:cl #:sqlite)
  (:export 
   #:store
   #:store-count-rows
   #:store-get-column-labels
   #:store-get-rows
   ))

;;; ---------------------------------------------------------------------
;;; delectus.document
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Classes and functions for interacting with the Delectus document
;;; model

(defpackage #:delectus.document
  (:use #:cl)
  (:export 
   #:document
   #:store
   #:visible-column-labels
   #:visible-rows
   ))

;;; ---------------------------------------------------------------------
;;; delectus.command
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; Classes and functions that command the Delectus document to carry
;;; out user instructions

(defpackage #:delectus.command
  (:use #:cl)
  (:export 
   #:command
   #:insert-row-command
   #:undo-insert-row-command
   #:update-row-command
   #:undo-update-row-command
   #:delete-row-command
   #:undo-delete-row-command
   #:add-column-command
   #:undo-add-column-command
   #:delete-column-command
   #:undo-delete-column-command
   #:move-column-command
   #:undo-move-column-command
   ))

;;; ---------------------------------------------------------------------
;;; delectus.capi
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; On macOS, Linux, and Windows platforms, classes and functions that
;;; provide the Delectus graphical user interface. Uses and requires
;;; Lispworks CAPI

#+capi
(defpackage #:delectus.capi
  (:use #:cl #:capi))

