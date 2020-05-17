;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-macos.asd
;;;; Project:       delectus 2
;;;; Purpose:       delectus-macos: the Mac UI app for Delectus 2
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:delectus-macos
  :description "The macOS Delectus 2 app"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "2.0.3"
  :serial t
  :depends-on (:delectus)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:module "macos"
                                  :serial t
                                  :components
                                  (;; CAPI UI
                                   (:file "package")             ; the ui package
                                   (:file "macos-constants")   ; constants to control UI appearance
                                   (:file "macos-view-utils")  ; operations on native macOS widgets
                                   (:file "views-items-sheet") ; spreadsheet-like view
                                   ))))))

(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-delectus-macos ()
  (let ((project-libdir
         #+(or :mac :os-macosx) (merge-pathnames "delivery/macos/lib/" $project-root)
         #+:linux (merge-pathnames "delivery/linux/ubuntu/x86_64/lib/" $project-root)
         #-(or :mac :os-macosx :linux) nil
         ))
    (pushnew project-libdir
             cffi:*foreign-library-directories*
             :test #'equal)
    (asdf:load-system :delectus-macos)))


;;; (cl-user::load-delectus-macos)
