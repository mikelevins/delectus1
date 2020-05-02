;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.asd
;;;; Project:       delectus 2
;;;; Purpose:       delectus system definition
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(ql:quickload :cffi)

(asdf:defsystem #:delectus
  :description "The Delectus 2 data engine"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "2.0.3"
  :serial t
  :depends-on (:fset :fare-csv :uuid :sqlite :cl-interpol :cl-emb :jonathan :local-time :named-readtables)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-parameters") ; application globals
                                     (:file "system-syntax")     ; syntax for map literals
                                     (:file "system-bind")       ; more compact binding of multiple values
                                     (:file "system-utils")      ; general-purpose helpers
                                     (:file "data-identities")   ; Delectus-specific UUID format
                                     (:file "data-csv")          ; reading and writing CSV files
                                     (:file "data-json")         ; reading and writing JSON data
                                     (:file "store-columns")     ; modeling Delectus columns
                                     (:file "store-sql-utils")   ; helpers for sqlgen
                                     (:file "store-sqlgen")      ; generating SQL for the Dlectus store
                                     (:file "store-sqlite")      ; operating on SQLite files
                                     (:file "store-model")       ; store operations on Delectus model objects
                                     ;; Test data
                                     (:file "test-data")))))

(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-delectus ()
  (let ((project-libdir (merge-pathnames "delivery/macos/lib/" $project-root)))
    (pushnew project-libdir
             cffi:*foreign-library-directories*
             :test #'equal)
    (asdf:load-system :delectus)))


;;; (cl-user::load-delectus)
