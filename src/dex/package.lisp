;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Delectus Dex
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; delectus.dex
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; the package for the delectus data engine

(defpackage :delectus.dex
  (:use :cl :sqlite :fare-csv :delectus.data #+capi :capi))




