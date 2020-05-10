;;;; ***********************************************************************
;;;;
;;;; Name:          xdelectus.asd
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
  :depends-on (:fset :fare-csv :osicat :uuid :sqlite :jonathan :local-time :sxql)
  :components ((:module "xsrc"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "system-syntax")      ; syntax for map literals
                                     (:file "system-parameters")  ; application globals
                                     (:file "system-bind")        ; more compact binding of multiple values
                                     (:file "system-utils")       ; general-purpose helpers
                                     (:file "data-identities")    ; delectus-specific UUID format
                                     (:file "system-node")          ; identity for the combo of device+user
                                     (:file "data-origins")       ; distinguishing ops from different files and accounts
                                     ))))

(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-xdelectus ()
  (let ((project-libdir (merge-pathnames "delivery/macos/lib/" $project-root)))
    (pushnew project-libdir
             cffi:*foreign-library-directories*
             :test #'equal)
    (asdf:load-system :delectus)))

;;; (cl-user::load-xdelectus)
