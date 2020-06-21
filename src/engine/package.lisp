;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       delectus 2
;;;; Purpose:       delectus package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage #:delectus
  (:use #:cl #:sqlite #:sxql)
  (:import-from #:fset
                #:compare #:convert #:do-map #:domain
                #:lookup #:wb-map #:with)
  (:import-from #:local-time #:now)
  (:export
   ))

