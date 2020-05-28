;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.asd
;;;; Project:       delectus 2
;;;; Purpose:       Delectus 2 system definition
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(ql:quickload :cffi)

(asdf:defsystem #:delectus
  :description "Delectus 2"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "2.0.7"
  :serial t
  :depends-on (:fset :fare-csv :cl-intbytes :binascii :uuid :sqlite :jonathan :local-time :sxql)
  :components ((:module "xsrc"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-syntax")         ; syntax for map literals
                                     (:file "system-bind")           ; more compact binding of multiple values
                                     (:file "system-utils")          ; general-purpose helpers
                                     (:file "system-parameters")     ; application globals
                                     (:file "data-identities")       ; delectus-specific UUID format
                                     (:file "system-node")           ; fetching and saving the node id
                                     ))))

(defun load-delectus ()
  (asdf:load-system :delectus))

;;; (cl-user::load-delectus)
