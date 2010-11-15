;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          application.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the main Delectus application
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; application main
;;; ---------------------------------------------------------------------

(defun delectus ()
  (init-platform-libraries)
  (init-platform-interface)
  (convert-to-screen nil))
