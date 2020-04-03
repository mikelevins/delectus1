;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       delectus 2
;;;; Purpose:       delectus package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:delectus-ops
  (:nicknames #:op)
  (:use #:cl))

(defpackage #:delectus
  (:use #:cl #:sqlite #:sxql))
