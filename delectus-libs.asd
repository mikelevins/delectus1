;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-libs.asd
;;;; Project:       delectus 2
;;;; Purpose:       Support linraries for Delectus 2
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(ql:quickload :cffi)

;;; We create an empty defsystem so that we can tell the delectus
;;; system to look for libraries in a path relative to this system

(asdf:defsystem #:delectus-libs
  :description "Base path for Delectus 2 supporting libraries"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "2.0.7"
  :serial t
  :depends-on ()
  :components ())

;;; define a package and a parameter for the library path

(defpackage #:delectus-libs
  (:use #:cl)
  (:export #:*delectus-libraries-path*))

(in-package :delectus-libs)

(defparameter *delectus-libraries-path*
  #+(and :lispworks :mac)(asdf:system-relative-pathname :delectus-libs #p"product/macos/lib/")
  #+:linux (asdf:system-relative-pathname :delectus-libs #p"product/linux/ubuntu/x86_64/lib/")
  )

;;; now push the needed path onto CFFI's list of library search paths

(in-package :cl-user)

(pushnew delectus-libs::*delectus-libraries-path*
         cffi:*foreign-library-directories*
         :test #'equal)
