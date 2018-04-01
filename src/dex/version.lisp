;;;; ***********************************************************************
;;;;
;;;; Name:          version.lisp
;;;; Project:       Delectus Dex
;;;; Purpose:       Dex version
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.dex)

;;; ---------------------------------------------------------------------
;;; +dex-major-version+
;;; ---------------------------------------------------------------------
;;;
;;; major version of the Dex data engine release

(defparameter +dex-major-version+ 2)

;;; ---------------------------------------------------------------------
;;; +dex-minor-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; minor version of the Dex data engine release

(defparameter +dex-minor-version+ 0)

;;; ---------------------------------------------------------------------
;;; +dex-patch-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; patch version of the Dex data engine release

(defparameter +dex-patch-version+ 0)

;;; ---------------------------------------------------------------------
;;; dex-version ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Dex data engine release as a
;;; vector of integers

(defun dex-version ()
  (vector +dex-major-version+
          +dex-minor-version+
          +dex-patch-version+))

;;; ---------------------------------------------------------------------
;;; dex-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Dex data engine release as a string

(defun dex-version-string ()
  (format nil "~A.~A.~A"
          +dex-major-version+
          +dex-minor-version+
          +dex-patch-version+))

;;; ---------------------------------------------------------------------
;;; +dex-format-major-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; major version of the Dex file format

(defparameter +dex-format-major-version+ 2)

;;; ---------------------------------------------------------------------
;;; +dex-format-minor-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; minor version of the Dex file format

(defparameter +dex-format-minor-version+ 0)

;;; ---------------------------------------------------------------------
;;; dex-format-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Dex file format as a string

(defun dex-format-version-string ()
  (format nil "~A.~A"
          +dex-format-major-version+
          +dex-format-minor-version+))
