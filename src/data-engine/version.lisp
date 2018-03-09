;;;; ***********************************************************************
;;;;
;;;; Name:          version.lisp
;;;; Project:       Delectus 2 data engine
;;;; Purpose:       delectus version
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :data)

;;; ---------------------------------------------------------------------
;;; +delectus-data-major-version+
;;; ---------------------------------------------------------------------
;;;
;;; major version of the Delectus data engine release

(defparameter +delectus-data-major-version+ 2)

;;; ---------------------------------------------------------------------
;;; +delectus-data-minor-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; minor version of the Delectus data engine release

(defparameter +delectus-data-minor-version+ 0)

;;; ---------------------------------------------------------------------
;;; +delectus-data-patch-version+
;;; ---------------------------------------------------------------------
;;; *exported special variable*
;;;
;;; patch version of the Delectus data engine release

(defparameter +delectus-data-patch-version+ 4)

;;; ---------------------------------------------------------------------
;;; delectus-data-version ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus data engine release as a
;;; vector of integers

(defun delectus-data-version ()
  (vector +delectus-data-major-version+
          +delectus-data-minor-version+
          +delectus-data-patch-version+))

;;; ---------------------------------------------------------------------
;;; delectus-data-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus data engine release as a string

(defun delectus-data-version-string ()
  (format nil "~A.~A.~A"
          +delectus-data-major-version+
          +delectus-data-minor-version+
          +delectus-data-patch-version+))

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

(defparameter +delectus-format-minor-version+ 1)

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
