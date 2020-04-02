;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.asd
;;;; Project:       delectus 2
;;;; Purpose:       delectus system definition
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;;; delectus.asd

(asdf:defsystem #:delectus
  :description "Describe delectus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:fset :fare-csv :uuid :sqlite :sxql :jonathan :local-time)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "identities")
                                     (:file "csv")
                                     (:file "sqlite")
                                     (:file "node")
                                     (:file "op")
                                     (:file "delectus-file")))))

;;; (asdf:load-system :delectus)
