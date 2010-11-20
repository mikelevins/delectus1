;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       document object
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; document
;;; ---------------------------------------------------------------------

(defclass document ()
  ((title)
   (pathname)
   (model)
   (window)))
