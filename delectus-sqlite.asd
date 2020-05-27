;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sqlite.asd
;;;; Project:       delectus 2
;;;; Purpose:       load our specific version of the sqlite library
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem #:delectus-sqlite
  :description "SQLite 3 for Delectus 2"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "2.0.7"
  :serial t
  :depends-on (:iterate :cffi)
  :components
  ((:module "local"
            :serial t
            :components
            ((:module "src"
                      :serial t
                      :components
                      ((:module "cl-sqlite-20190813-git"
                                :serial t
                                :components
                                ((:file "sqlite-ffi")
                                 (:file "cache")
                                 (:file "sqlite")))))))))
