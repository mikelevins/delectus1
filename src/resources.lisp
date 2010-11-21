;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          resources.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       resource definitions
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
      #+cocoa
      (concatenate 'string
                   (invoke-into 'string (invoke "NSBundle" "mainBundle") "resourcePath")
                   "/")
      #+win32
      (namestring (merge-pathnames "Resources/"
                   (make-pathname :directory (pathname-directory (pathname (lisp-image-name))))))
      (cl-user::path "template/Delectus.app/Contents/Resources/")))

(defun resource (p)
  (namestring (merge-pathnames p (resource-path))))

(defmethod define-resource (category name path)
  (let* ((resmap (resources (app)))
         (catmap (map:get resmap category :default {})))
    (setf (resources (app))
          (map:associate resmap category
                     (map:associate catmap name path)))))

(defmethod find-resource (category name)
  (map:get (map:get (resources (app)) category :default {}) name :default nil))

(defun image (name)
  (find-resource :images name))

(defmethod init-resources ()
  #+cocoa(define-resource :images :add-button (resource "add.png"))
  #+win32(define-resource :images :add-button (resource "add.bmp"))
  #+cocoa(define-resource :images :del-button (resource "del.png"))
  #+win32(define-resource :images :del-button (resource "del.bmp"))
  #+cocoa(define-resource :images :trashempty-button (resource "trashempty32.png"))
  #+win32(define-resource :images :trashempty-button (resource "trashempty32.bmp")))

