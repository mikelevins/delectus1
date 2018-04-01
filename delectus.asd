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
;;; delectus 2 data engine
;;; ---------------------------------------------------------------------

(asdf:defsystem #:data-engine
    :description "The Delectus data engine"
    :version "2.0.0"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache 2.0"
    :serial t
    :depends-on (:sqlite :fare-csv)
    :components ((:module "src" :serial t
                          :components ((:module "data-engine" :serial t
                                                :components ((:file "package")
                                                             (:file "version")
                                                             (:file "utils-file")
                                                             (:file "sqlite")
                                                             ))))))

;;; (asdf:load-system :data-engine)


;;; ---------------------------------------------------------------------
;;; delectus 2 desktop: a list manager
;;; ---------------------------------------------------------------------

(asdf:defsystem #:delectus-desktop
  :description "Delectus desktop UI"
  :version "2.0.0"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:data-engine)
  :components ((:module "src" :serial t
                        :components ((:module "ui-capi" :serial t
                                              :components ((:file "package")
                                                           (:file "version")
                                                           (:file "controllers-capi")
                                                           (:file "views-capi")
                                                           ))))))



;;; (asdf:load-system :delectus-desktop)
;;; (env:start-environment)

;;; ---------------------------------------------------------------------
;;; delectus Dex: a data app builder and runner
;;; ---------------------------------------------------------------------

(asdf:defsystem #:dex
  :description "Delectus Dex: the Deck runner"
  :version "2.0.0"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:data-engine)
  :components ((:module "src" :serial t
                        :components ((:module "dex" :serial t
                                              :components ((:file "package")
                                                           (:file "version")
                                                           ))))))



;;; (asdf:load-system :dex)
;;; (env:start-environment)


(asdf:defsystem #:dexter
  :description "Delectus Dexter: the Deck builder"
  :version "2.0.0"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:data-engine)
  :components ((:module "src" :serial t
                :components ((:module "dex" :serial t
                              :components ((:file "package")
                                           (:file "version")
                                           ))))))



;;; (asdf:load-system :dexter)
;;; (env:start-environment)
