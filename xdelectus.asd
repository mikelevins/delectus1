
;;;; ***********************************************************************
;;;;
;;;; Name:          xdelectus.asd
;;;; Project:       delectus 2
;;;; Purpose:       experiment in alternative storage strategy
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;;; xdelectus.asd

(ql:quickload :cffi)

(asdf:defsystem #:xdelectus
  :description "Describe xdelectus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "2.0.0"
  :serial t
  :depends-on (:fset :fare-csv :uuid :sqlite :cl-emb :jonathan :local-time)
  :components ((:module "xsrc"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-parameters") ; application globals
                                     (:file "system-syntax")     ; syntax for map literals
                                     (:file "system-bind")       ; more compact binding of multiple values
                                     (:file "system-utils")      ; general-purpose helpers
                                     (:file "data-identities")   ; Delectus-specific UUID format
                                     (:file "data-json")         ; reading and writing JSON data
                                     (:file "store-column-info") ; sqlite column-info utils
                                     (:file "store-sqlite")      ; operating on SQLite files
                                     (:file "store-sqlgen")      ; generating SQL for interacting with the Delectus store
                                     (:file "store-model")       ; store operations on Delectus model objects
                                     ;;(:file "data-csv")          ; reading and writing CSV files
                                     ;;(:file "store-columns")     ; modeling Delectus columns
                                     ;;(:file "store-sql-utils")   ; helpers for sqlgen
                                     ;;(:file "store-sqlgen")      ; generating SQL for the Dlectus store
                                     ;;(:file "store-model")       ; store operations on Delectus model objects
                                     ;; CAPI UI
                                     ;;(:file "macos-constants")   ; constants to control macOS UI appearance
                                     ;;(:file "macos-view-utils")  ; operations on native macOS widgets
                                     ;;(:file "views-items-sheet") ; spreadsheet-like view
                                     ;;(:file "views-card-list")   ; list-of-cards view
                                     ;; Test data
                                     ;;(:file "test-data")
                                     ))))

(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-xdelectus ()
  (let ((project-libdir (merge-pathnames "delivery/macos/lib/" $project-root)))
    (pushnew project-libdir
             cffi:*foreign-library-directories*
             :test #'equal)
    (asdf:load-system :xdelectus)))


;;; (cl-user::load-xdelectus)
