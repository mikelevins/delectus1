;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reousrces.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       platform-independent resource access
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defun resource (p)
  (namestring (merge-pathnames p (resource-path))))
