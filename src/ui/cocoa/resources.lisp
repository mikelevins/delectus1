;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       cocoa-specific resources
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; resources
;;; ---------------------------------------------------------------------

(defun resource-path ()
  (if (cl-user::delivered?)
      (concatenate 'string
                   (invoke-into 'string (invoke "NSBundle" "mainBundle") "resourcePath")
                   "/")
      (cl-user::path "template/Delectus.app/Contents/Resources/")))

(defun nsimage (path)
  (invoke (invoke "NSImage" "alloc") "initByReferencingFile:" path))

;;;(ns-image (resource "trashempty48.png"))