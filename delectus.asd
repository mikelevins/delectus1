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
  :version "2.0.6"
  :serial t
  :depends-on (:fset :fare-csv :osicat :cl-intbytes :uuid :sqlite :jonathan :local-time :sxql)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-syntax")         ; syntax for map literals
                                     (:file "system-parameters")     ; application globals
                                     (:file "system-node")           ; saving and fetching the node id
                                     (:file "system-bind")           ; more compact binding of multiple values
                                     (:file "system-utils")          ; general-purpose helpers
                                     (:file "data-identities")       ; delectus-specific UUID format
                                     ;; (:file "data-origins")          ; identifiers for Delectus ops
                                     ;; (:file "data-column-labels")    ; autogenerated labels for userdata columns
                                     ;; (:file "data-timestamps")       ; Delectus-specific timestamps
                                     ;; (:file "store-column-info")     ; sqlite column-info utils
                                     ;; (:file "store-sqlite")          ; operating on SQLite files
                                     ;; (:file "data-columns")          ; utilities for working with column descriptions
                                     ;; (:file "store-sqlgen")          ; SQL for interacting with the Delectus store
                                     ;; (:file "store-delectus-table")  
                                     ;; (:file "store-listnames-table") 
                                     ;; (:file "store-comments-table")  
                                     ;; (:file "store-columns-table")   
                                     ;; (:file "store-items-table")     
                                     ;; (:file "store-listfile")        ; operations on Delectus list files
                                     ;; (:file "data-csv")           ; reading and writing CSV files
                                     ;; ;; ;; making test data
                                     ;; (:file "test-data")
                                     ))))

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
