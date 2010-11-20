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

(defsystem delectus-data
  :name "delectus data"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Data Layer"
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :binary-types)
  :serial t
  :components ((:module lib :serial t
                        :components ((:file "parse-number")
                                     (:file "csv-parser")))
               (:module src :serial t
                        :components
                        ((:file "package")
                         (:file "model")
                         (:file "storage")
                         (:file "csv")
                         ))))

(defsystem delectus-cocoa
  :name "delectus cocoa"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Cocoa Applcation"
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :delectus-data)
  :serial t
  :components ())


(in-package :cl-user)

(defun load-data ()(asdf:oos 'asdf:load-op :delectus-data))
(defun load-cocoa ()(asdf:oos 'asdf:load-op :delectus-cocoa))

;;; (load-data)
