;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.asd
;;;; Project:       Delectus 2
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; dev-time path utils
;;; ---------------------------------------------------------------------

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(defun path (p)(merge-pathnames p (path-base)))

(defun add-to-asdf (path)
  (pushnew (truename (merge-pathnames path (path-base)))
           asdf:*central-registry* :test 'equalp))

;;; ---------------------------------------------------------------------
;;; whether the running lisp is a delivered app
;;; ---------------------------------------------------------------------

(let ((delivered? nil))
  (defun set-delivered (y-or-n)
    (setf delivered? y-or-n))
  (defun delivered? ()
    delivered?))

;;; ---------------------------------------------------------------------
;;; system definition and loader
;;; ---------------------------------------------------------------------

(defpackage #:delectus-asd
  (:use :cl :asdf))

(in-package :delectus-asd)

(defsystem delectus
  :name "delectus"
  :version "2.0a1"
  :author "mikel evins"
  :description "Delectus 2"
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :cl-store)
  :serial t
  :components ((:module lib :serial t
                        :components ((:file "parse-number")
                                     (:file "csv-parser")))
               (:module src :serial t
                        :components
                        ((:file "package")
                         (:file "model")
                         (:file "csv")))))


(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
