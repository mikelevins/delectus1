;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-capi-macos.asd
;;;; Project:       delectus 2
;;;; Purpose:       delectus-capi-macos: the Mac UI app for Delectus 2
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:delectus-capi-macos
    :description "The macOS Delectus 2 app"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2.0"
    :version "2.0.7"
    :serial t
    :depends-on (:delectus-engine)
    :components
    ((:module "src"
              :serial t
              :components
              ((:module "capi"
                        :serial t
                        :components
                        ((:module "macos"
                                  :serial t
                                  :components
                                  (;; CAPI UI
                                   (:file "package")              ; the ui package
                                   (:file "macos-constants")      ; constants to control UI appearance
                                   (:file "view-utils")           ; general view helpers
                                   (:file "macos-view-utils") ; tweaks for native macOS widgets
                                   (:file "view-value-pane")       ; a single-item card-style pane
                                   (:file "view-item-card")       ; a single-item card-style pane
                                   (:file "view-item-row")        ; a single-item row-style pane
                                   (:file "view-items-sheet")     ; a spreadhseet view
                                   (:file "view-app-menus")    ; the app menubar
                                   (:file "app-main")          ; toplevel macOS application
                                   ))))))))



(defparameter $project-root (make-pathname :directory (pathname-directory *load-pathname*)))

;;; push the project lib directory onto cffi:*foreign-library-directories*
;;; before loading, so we get the project-specific version of SQLite
(defun load-delectus-capi-macos ()
  (asdf:load-system :delectus-capi-macos))


;;; (cl-user::load-delectus-capi-macos)
