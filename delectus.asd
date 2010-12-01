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
;;; system definitions and loaders
;;; ---------------------------------------------------------------------

(defpackage #:delectus-asd
  (:use :cl :asdf))

(in-package :delectus-asd)

(defsystem delectus-list-engine
  :name "delectus list engine"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Data Layer"
  :depends-on (:folio.as :folio.functions :folio.collections :cl-prevalence)
  :serial t
  :components ((:module lib :serial t
                        :components ((:file "parse-number")
                                     (:file "csv-parser")))
               (:module src :serial t
                        :components
                        ((:file "package")
                         (:module list-engine :serial t
                                  :components
                                  ((:file "model")
                                   (:file "csv")
                                   (:file "sort")
                                   (:file "presentation")
                                   (:file "store")))))))

;;; (asdf::oos 'asdf:load-op :delectus-list-engine)

(defsystem delectus-cocoa
  :name "delectus cocoa app"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Cocoa Application"
  :depends-on (:delectus-list-engine)
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "singleton")
                         (:file "resources")
                         (:module controllers :serial t
                                  :components
                                  ((:file "document")
                                   (:file "app")))
                         (:module views :serial t
                                  :components
                                  ((:file "menus")
                                   (:file "delectus-list-pane")
                                   (:file "document-window")
                                   (:file "ui")))))))

(defsystem delectus-win32
  :name "delectus win32 app"
  :version "1.9a2"
  :author "mikel evins"
  :description "Delectus Windows Applcation"
  :depends-on (:delectus-list-engine)
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "singleton")
                         (:file "resources")
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

(defun load-cocoa-app ()(asdf:oos 'asdf:load-op :delectus-cocoa))
(defun load-win32-app ()(asdf:oos 'asdf:load-op :delectus-win32))

;;; (load-cocoa-app)
;;; (load-win32-app)
;;; (delectus::init-resources)