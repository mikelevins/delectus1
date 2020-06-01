;;;; ***********************************************************************
;;;;
;;;; Name:          system-process-identity.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a unique ID for a running Delectus process
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an identity computed when a Delectus process starts. It serves as
;;; a unique identifier for a particular Delectus session, used in
;;; computing origin values.

(let ((%process-identity nil))
  (defun process-identity ()
    (or %process-identity
        (progn (setf %process-identity (makeid))
               %process-identity))))
