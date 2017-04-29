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
;;; delectus 2: a list manager
;;; ---------------------------------------------------------------------

(asdf:defsystem #:delectus
  :description "Delectus: a list manager"
  :version "2.0.0"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:sqlite :fare-csv :local-time)
  :components ((:module "src" :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system")
                                     (:file "utilities")
                                     (:file "file")
                                     (:file "legacy")
                                     (:file "store")
                                     (:file "document")
                                     (:file "command")
                                     #+capi (:file "view-capi")
                                     ))))

;;; (asdf:load-system :delectus)
