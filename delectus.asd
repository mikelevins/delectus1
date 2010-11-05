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
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :fare-csv)
  :components ((:module src :serial t
                        :components ((:file "package")
                                     (:module common :serial t
                                              :components ((:file "model")
                                                           (:file "singleton")
                                                           (:file "serialization")
                                                           (:file "csv")
                                                           (:file "presentation")
                                                           (:file "app")))
                                     #+cocoa
                                     (:module cocoa :serial t
                                              :components ((:file "resources")
                                                           (:file "views")
                                                           (:file "menus")
                                                           (:file "delectus-window")
                                                           (:file "document")
                                                           (:file "macos-application-bundle")))))))

(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
;;; (delectus::open-document "/Applications/factor/extra/usa-cities/zipcode.csv")
;;; (delectus::new-untitled-document)

