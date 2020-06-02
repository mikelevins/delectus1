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
  :version "2.0.9"
  :serial t
  :depends-on (:fset :fare-csv :cl-intbytes :binascii :uuid :sqlite :jonathan :local-time :sxql)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-syntax")            ; syntax for map literals
                                     (:file "system-bind")              ; more compact binding of multiple values
                                     (:file "system-utils")             ; general-purpose helpers
                                     (:file "system-parameters")        ; application globals
                                     (:file "data-identities")          ; delectus-specific UUID format
                                     (:file "data-column-labels")       ; column-label strings
                                     (:file "system-process-identity")  ; unique id of a running Delectus session
                                     (:file "data-origins")             ; identifiers for Delectus ops
                                     (:file "data-timestamps")          ; Delectus-specific timestamps
                                     (:file "store-column-info")        ; column-info struct
                                     (:file "store-sqlite")             ; low-level SQLite operations
                                     (:file "data-column-descriptions") 
                                     ;; (:file "store-sqlgen")             ; generating SQL for use with list files
                                     ;; (:file "store-tables")             ; creating Delectus tables
                                     ;; (:file "store-ops")                ; inserting and fetching ops
                                     ;; (:file "store-listfile")           ; creating Delectus list files
                                     ;; (:file "data-csv")                 ; reading and writing CSV files
                                     ))))

(defun load-delectus ()
  (asdf:load-system :delectus))

;;; (cl-user::load-delectus)
