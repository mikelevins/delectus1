;;;; ***********************************************************************
;;;;
;;;; Name:          version.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       delectus version
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.desktop)

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

(defparameter +delectus-patch-version+ 3)

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
          +delectus-patch-version+))

;;; ---------------------------------------------------------------------
;;; delectus-version-string ()
;;; ---------------------------------------------------------------------
;;; *exported function*
;;;
;;; returns the version of the Delectus release as a string

(defun delectus-version-string ()
  (format nil "~A.~A.~A"
          +delectus-major-version+
          +delectus-minor-version+
          +delectus-patch-version+))
