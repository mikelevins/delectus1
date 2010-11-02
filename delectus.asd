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

(let ((delivered? nil))
  (defun set-delivered (y-or-n)
    (setf delivered? y-or-n))
  (defun delivered? ()
    delivered?))

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
                                       (:file "resources")
                                       (:file "model")
                                       (:file "csv")
                                       (:file "presentation")
                                       (:file "views")
                                       (:file "menus")
                                       (:file "delectus-window")
                                       (:file "document")
                                       (:file "singleton")
                                       (:file "app")))))

(in-package :cl-user)

(defun load-delectus ()
  (asdf:oos 'asdf:load-op :delectus))

;;; (load-delectus)
;;; (delectus::open-document "/Applications/factor/extra/usa-cities/zipcode.csv")
;;; (delectus::open-document nil)
