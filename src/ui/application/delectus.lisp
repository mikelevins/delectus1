;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the main Delectus application
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defun run-delectus ()
  (new-untitled-document))

(defun delectus ()
  (init-platform-libraries)
  (init-platform-interface)
  (run-delectus))

;;; (delectus)