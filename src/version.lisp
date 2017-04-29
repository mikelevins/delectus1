;;;; ***********************************************************************
;;;;
;;;; Name:          version.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       delectus version
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.version)

;;; ---------------------------------------------------------------------
;;; +delectus-major-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; major version of the Delectus release

(defparameter +delectus-major-version+ 2)

;;; ---------------------------------------------------------------------
;;; +delectus-minor-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; minor version of the Delectus release

(defparameter +delectus-minor-version+ 0)

;;; ---------------------------------------------------------------------
;;; +delectus-patch-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; patch version of the Delectus release

(defparameter +delectus-patch-version+ 1)

;;; ---------------------------------------------------------------------
;;; delectus-version ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus release as a vector of
;;; integers

(defun delectus-version ()
  (vector +delectus-major-version+
          +delectus-minor-version+
          +delectus-patch-version+
          +delectus-build-number+))

;;; ---------------------------------------------------------------------
;;; delectus-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus release as a string

(defun delectus-version-string ()
  (format nil "~A.~A.~A (build ~A)"
          +delectus-major-version+
          +delectus-minor-version+
          +delectus-patch-version+
          +delectus-build-number+))

;;; ---------------------------------------------------------------------
;;; +delectus-format-major-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; major version of the Delectus file format

(defparameter +delectus-format-major-version+ 5)

;;; ---------------------------------------------------------------------
;;; +delectus-format-minor-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; minor version of the Delectus file format

(defparameter +delectus-format-minor-version+ 0)

;;; ---------------------------------------------------------------------
;;; delectus-format-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus file format as a string

(defun delectus-format-version-string ()
  (format nil "~A.~A"
          +delectus-format-major-version+
          +delectus-format-minor-version+))
