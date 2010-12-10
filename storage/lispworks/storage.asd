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

(ql:quickload :cffi)
(pushnew #P"/usr/local/lib/" cffi:*foreign-library-directories*
         :test #'equal)
(ql:quickload :sqlite)

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
;;; system definitions and loaders
;;; ---------------------------------------------------------------------

(defpackage #:delectus-asd
  (:use :cl :asdf))

(in-package :delectus-asd)

(defsystem delectus-storage-engine
  :name "delectus storage engine"
  :version "1.9a3"
  :author "mikel evins"
  :description "Delectus Storage Engine"
  :depends-on (:folio.as :folio.functions :folio.collections :cl-prevalence)
  :serial t
  :components ((:module lib :serial t
                        :components ((:file "parse-number")
                                     (:file "csv-parser")))
               (:module src :serial t
                        :components
                        ((:file "package")
                         (:file "sqlite")
                         ;;(:file "take-labels")
                         ;;(:file "delectus")
                         ;;(:file "csv")
                         ;;(:file "sort")
                         ))))

;;; (asdf::oos 'asdf:load-op :delectus-storage-engine)
