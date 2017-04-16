;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.asd
;;;; Project:       Delectus 2
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(asdf:defsystem #:delectus
    :description "Delectus: a list manager"
    :author "mikel evins <mevins@me.com>"
    :license "Apache 2.0" :serial t
    :depends-on (:sqlite :local-time)
    :components ((:module "src" :serial t
                          :components ((:file "package")
                                       (:file "version")
                                       (:file "legacy")
                                       (:file "engine")
                                       (:file "capi")))))


;;; (asdf:load-system :delectus)
