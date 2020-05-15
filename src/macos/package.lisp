;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       delectus 2
;;;; Purpose:       delectus-ui package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage #:delectus-ui
  (:nicknames #:ui)
  (:use #:cl #:sqlite #:delectus #:capi))
