;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          paclage.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "DELECTUS"
  (:use :cl :capi #+cocoa :objc :folio.as)
  (:shadow "ELEMENT")
  (:import-from :folio.fn #:$ #:^)
  (:import-from :cl-user))

