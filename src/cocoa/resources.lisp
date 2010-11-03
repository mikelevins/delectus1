(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; resources
;;; ---------------------------------------------------------------------

(defun resource-path ()
  (if (cl-user::delivered?)
      (invoke-into 'string (invoke "NSBundle" "mainBundle")
                   "resourcePath")
      (cl-user::resource "")))

(defun resource (p)
  (namestring (merge-pathnames p (cl-user::path "Contents/Resources/"))))

(defun ns-image (path)
  (invoke (invoke "NSImage" "alloc") "initByReferencingFile:" path))

;;;(ns-image (resource "images/trashempty48.png"))