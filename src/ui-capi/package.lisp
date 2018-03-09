;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; desktop
;;; ---------------------------------------------------------------------
;;; *package*
;;;
;;; the top-level package for the delectus desktop application

(defpackage :delectus.desktop
  (:use :cl :sqlite :fare-csv :delectus.data #+capi :capi))



