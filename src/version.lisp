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

(defparameter +delectus-major-version+ 2)
(defparameter +delectus-minor-version+ 0)
(defparameter +delectus-patch-version+ 1)
(defparameter +delectus-build-number+ 189)

(defun delectus-version ()
  (vector +delectus-major-version+
          +delectus-minor-version+
          +delectus-patch-version+
          +delectus-build-number+))

(defun delectus-version-string ()
  (format nil "~A.~A.~A (build ~A)"
          +delectus-major-version+
          +delectus-minor-version+
          +delectus-patch-version+
          +delectus-build-number+))

(defparameter +delectus-format-version+ 5)

(defun delectus-format-version-string ()
  (format nil "~A.~A"
          +delectus-format-version+))