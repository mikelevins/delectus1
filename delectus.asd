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
  :depends-on (:delectus-libs :fset :fare-csv :cl-intbytes :binascii :uuid :sqlite :jonathan :local-time :sxql)
  :components ((:module "src"
                        :serial t
                        :components ())))

(defun load-delectus ()
  (asdf:load-system :delectus))

;;; (cl-user::load-delectus)
