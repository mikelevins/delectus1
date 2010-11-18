(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; dev-time path utils
;;; ---------------------------------------------------------------------

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(defun path (p)(merge-pathnames p (path-base)))

(defun add-to-asdf (path)
  (pushnew (truename (merge-pathnames path (path-base)))
           asdf:*central-registry* :test 'equalp))

(add-to-asdf "lib/folio/lib/misc-extensions_1.2.0/")
(add-to-asdf "lib/folio/lib/fset_1.2.2/")
(add-to-asdf "lib/folio/as/")
(add-to-asdf "lib/folio/boxes/")
(add-to-asdf "lib/folio/functions/")
(add-to-asdf "lib/folio/collections/")

;;; ---------------------------------------------------------------------
;;; whether the running lisp is a delivered app
;;; ---------------------------------------------------------------------

(let ((delivered? nil))
  (defun set-delivered (y-or-n)
    (setf delivered? y-or-n))
  (defun delivered? ()
    delivered?))

;;; ---------------------------------------------------------------------
;;; system definition and loader
;;; ---------------------------------------------------------------------

(defpackage #:delectus-asd
  (:use :cl :asdf))

(in-package :delectus-asd)

(defsystem delectus
  :name "delectus"
  :version "2.0a1"
  :author "mikel evins"
  :description "Delectus 2"
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :fare-csv :cl-store)
  :components ((:module src :serial t
                        :components ((:module common :serial t
                                              :components
                                              ((:file "package")
                                               (:file "singleton")
                                               (:module data :serial t
                                                        :components
                                                        ((:file "model")
                                                         (:file "csv")
                                                         (:file "presentation")))))
                                     (:module ui :serial t
                                              :components ((:module common :serial t
                                                                    :components
                                                                    ((:file "resources")
                                                                     (:file "top-button")))
                                                           #+cocoa
                                                           (:module cocoa :serial t
                                                                    :components
                                                                    ((:file "init")
                                                                     (:file "constants")
                                                                     (:file "resources")
                                                                     ;;(:file "trash-button")
                                                                     ;;(:file "model-pane")
                                                                     (:file "application")
                                                                     (:file "document")))
                                                           #+win32
                                                           (:module win32 :serial t
                                                                    :components
                                                                    ((:file "init")
                                                                     (:file "resources")
                                                                     (:file "document")))
                                                           (:module application :serial t
                                                                    :components
                                                                    ((:file "delectus-window")
                                                                     (:file "document")
                                                                     (:file "application")
                                                                     (:file "delectus")))))))))


(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
;;; (delectus::delectus)