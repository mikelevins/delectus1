(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; resources
;;; ---------------------------------------------------------------------

(defun resource-path ()
  (if (cl-user::delivered?)
      (concatenate 'string
                   (invoke-into 'string (invoke "NSBundle" "mainBundle") "resourcePath")
                   "/")
      (cl-user::path "template/Delectus.app/Contents/Resources/")))

(defun resource (p)
  (namestring (merge-pathnames p (resource-path))))

(defun ns-image (path)
  (invoke (invoke "NSImage" "alloc") "initByReferencingFile:" path))

;;;(ns-image (resource "trashempty48.png"))