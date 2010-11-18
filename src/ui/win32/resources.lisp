;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       win32-specific resources
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; path to resources
;;; ---------------------------------------------------------------------

(defun resource-path ()
  (if (cl-user::delivered?)
      (merge-pathnames "resources/"
                       (make-pathname :directory (pathname-directory (pathname (lisp-image-name)))))
      (cl-user::path "template/Delectus.app/Contents/Resources/")))

;;; ---------------------------------------------------------------------
;;; resources used in Delectus
;;; ---------------------------------------------------------------------

(defparameter $add-button-image (resource "add.bmp"))
(defparameter $add-button-hl-image (resource "addhl.bmp"))
(defparameter $delete-button-image (resource "del.bmp"))
(defparameter $delete-button-hl-image (resource "delhl.bmp"))

