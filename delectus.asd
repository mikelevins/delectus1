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
  :license  "Specify license here"
  :version "2.0.0"
  :serial t
  :depends-on (:fset :fare-csv :uuid :sqlite :jonathan :local-time)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-syntax")        ; syntax for map literals
                                     (:file "system-parameters")    ; application globals
                                     (:file "system-bind")          ; more compact binding of multiple values
                                     (:file "system-utils")         ; general-purpose helpers
                                     ;; (:file "data-identities")   ; Delectus-specific UUID format
                                     ;; (:file "data-origin")       ; computing identity of the Delectus process
                                     ;; (:file "data-timestamps")   ; Delectus-specific timestamps
                                     ;; (:file "data-json")         ; reading and writing JSON data
                                     ;; (:file "store-column-info") ; sqlite column-info utils
                                     ;; (:file "store-sqlite")      ; operating on SQLite files
                                     ;; (:file "store-sqlgen")      ; generating SQL for interacting with the Delectus store
                                     ;; (:file "store-model")       ; store operations on Delectus model objects
                                     ;;(:file "data-csv")          ; reading and writing CSV files
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
