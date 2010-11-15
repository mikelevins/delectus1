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
                                              :components ((:file "package")
                                                           (:file "singleton")
                                                           (:file "resources")
                                                           (:module data :serial t
                                                                    :components ((:file "model")
                                                                                 (:file "presentation")
                                                                                 (:file "csv")
                                                                                 (:file "serialization")))))
                                     (:module platform :serial t
                                              :components (#+cocoa
                                                           (:module cocoa :serial t
                                                                    :components ((:file "utilities")))
                                                           #+win32
                                                           (:module win32 :serial t
                                                                    :components ((:file "utilities")))))
                                     #|(:module ui :serial t
                                              :components (#+cocoa
                                                           (:module cocoa :serial t
                                                                    :components 
                                                                    ((:file "macos-application-bundle")))
                                                           #+win32
                                                           (:module win32 :serial t
                                                                    :components ())
                                                           (:module common :serial t
                                                                    :components ((:file "views")
                                                                                 (:file "menus")
                                                                                 (:file "document")
                                                                                 (:file "delectus-window")
                                                                                 (:file "delectus-ui")
                                                                                 (:file "application")))))|#
                                     ))))


(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
;;; (delectus::open-document "/Applications/factor/extra/usa-cities/zipcode.csv")
;;; (setq $doc (delectus::new-untitled-document))
;;; (delectus::active-interface (delectus::app))
