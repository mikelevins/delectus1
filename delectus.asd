(in-package :cl-user)

(require :asdf)

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(pushnew (truename (merge-pathnames "../../bard/folio/as/" (path-base))) asdf:*central-registry* :test 'equalp)
(pushnew (truename (merge-pathnames "../../bard/folio/boxes/" (path-base))) asdf:*central-registry* :test 'equalp)
(pushnew (truename (merge-pathnames "../../bard/folio/functions/" (path-base))) asdf:*central-registry* :test 'equalp)
(pushnew (truename (merge-pathnames "../../bard/folio/collections/" (path-base))) asdf:*central-registry* :test 'equalp)

(defun path (p)
  (merge-pathnames p (translate-logical-pathname (path-base))))
(defun resource (p)
  (merge-pathnames p (path "Contents/Resources/")))

(defpackage #:delectus-asd
  (:use :cl :asdf))

(in-package :delectus-asd)

(defsystem delectus
    :name "delectus"
    :version "2.0a1"
    :maintainer ""
    :author "mikel evins"
    :licence ""
    :description "Delectus 2"
    :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections :fare-csv)
    :components ((:module src
                          :serial t
                          :components ((:file "package")
                                       (:file "singleton")
                                       (:file "model")
                                       (:file "presentation")
                                       (:file "document")
                                       (:file "csv")
                                       (:file "menus")
                                       (:file "views")
                                       (:file "app")
                                       (:file "ui")
                                       ;;(:file "app-delegate")
                                       ;;(:file "cocoa-init")
                                       ;;(:file "button-handlers")
                                       ))))

(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
;;; (setq $doc (delectus::new-untitled-document (delectus::app)))
;;; (setq $mod (delectus::model $doc))