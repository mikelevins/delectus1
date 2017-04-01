;;;; ***********************************************************************
;;;;
;;;; Name:          version.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       delectus version
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

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
