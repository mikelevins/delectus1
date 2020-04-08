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
  :depends-on (:fset :fare-csv :uuid :sqlite :cl-emb :jonathan :local-time :named-readtables)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-parameters")
                                     (:file "syntax")
                                     (:file "bind")
                                     (:file "utils")
                                     (:file "identities")
                                     (:file "columns")
                                     (:file "csv")
                                     (:file "json")
                                     (:file "delectus-sql")
                                     (:file "sqlite")
                                     (:file "op")
                                     (:file "delectus-file")))))

;;; (asdf:load-system :delectus)

