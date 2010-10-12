(in-package :cl-user)

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

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
    :depends-on (:folio.as :folio.functions :folio.collections)
    :components ((:module src
                          :serial t
                          :components ((:file "package")
                                       (:file "model")
                                       (:file "data-source")
                                       (:file "ui")))))

(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)