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

(defsystem delectus-data-engine
  :name "delectus data engine"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Data Layer"
  :depends-on (:folio.as :folio.functions :folio.collections)
  :serial t
  :components ((:module lib :serial t
                        :components ((:file "parse-number")
                                     (:file "csv-parser")))
               (:module src :serial t
                        :components
                        ((:file "package")
                         (:module data-engine :serial t
                                  :components
                                  ((:file "model")
                                   (:file "storage")
                                   (:file "csv")))))))

(defsystem delectus-cocoa
  :name "delectus cocoa app"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Cocoa Application"
  :depends-on (:delectus-data-engine)
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "singleton")
                         (:module controllers :serial t
                                  :components
                                  ((:file "document")
                                   (:file "app")))
                         (:module views :serial t
                                  :components
                                  ((:file "menus")
                                   (:file "document-window")
                                   (:file "ui")))))))

(defsystem delectus-win32
  :name "delectus win32 app"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Windows Applcation"
  :depends-on (:delectus-data-engine)
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "singleton")
                         (:module controllers :serial t
                                  :components
                                  ((:file "document")
                                   (:file "app")))
                         (:module views :serial t
                                  :components
                                  ((:file "menus")
                                   (:file "document-window")
                                   (:file "ui")))))))


(in-package :cl-user)

(defun load-data-engine ()(asdf:oos 'asdf:load-op :delectus-data-engine))
(defun load-cocoa-app ()(asdf:oos 'asdf:load-op :delectus-cocoa))
(defun load-win32-app ()(asdf:oos 'asdf:load-op :delectus-win32))

;;; (load-data-engine)
;;; (load-cocoa-app)
;;; (load-win32-app)