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
  (:use :cl :as :fn)
  (:shadow #:count #:get #:element)
  (:import-from :folio.fn #:^)
  (:import-from :folio.functions #:flip #:partial))

