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
  (:use :cl :capi #+cocoa :objc :as)
  (:shadow #:element)
  (:import-from :folio.functions #:flip #:partial))

