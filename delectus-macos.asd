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
  :version "2.0.7"
  :serial t
  :depends-on (:delectus)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:module "macos"
                                  :serial t
                                  :components
                                  (;; CAPI UI
                                   (:file "package")           ; the ui package
                                   (:file "macos-constants")   ; constants to control UI appearance
                                   (:file "macos-view-utils")  ; operations on native macOS widgets
                                   (:file "view-item-card")    ; a single-item pane
                                   ;; (:file "view-app-menus")    ; the app menubar
                                   ;; (:file "view-items-sheet")  ; spreadsheet-like view
                                   ;; (:file "app-main")          ; toplevel macOS application
                                   ))))))

(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-delectus-macos ()
  (asdf:load-system :delectus-macos))


;;; (cl-user::load-delectus-macos)
