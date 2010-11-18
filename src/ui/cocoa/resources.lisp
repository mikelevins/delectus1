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
;;; path to resources
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

;;; ---------------------------------------------------------------------
;;; resources used in Delectus
;;; ---------------------------------------------------------------------

(defparameter $add-button-image (resource "add.png"))
(defparameter $add-button-hl-image (resource "addhl.png"))
(defparameter $delete-button-image (resource "del.png"))
(defparameter $delete-button-hl-image (resource "delhl.png"))
(defparameter $trash-button-empty-image (resource "trashempty48.png"))
(defparameter $trash-button-full-image (resource "trashfull48.png"))
