;;;; configuration.lisp
;;;; find configuration files and set configuration parameters

(in-package #:delectus-server)

;;; TODO: write a real way to configure these

;;; ---------------------------------------------------------------------
;;; webserver parameters
;;; ---------------------------------------------------------------------

(defun server-port () 9000)

;;; ---------------------------------------------------------------------
;;; webserver file paths
;;; ---------------------------------------------------------------------

(defun server-pathname (&optional (which :root))
  (%server-pathname which))

(defmethod %server-pathname (which)
  (error "Unrecognized server-pathname selector: ~S" which))

(defmethod %server-pathname ((which (eql :root)))
  #p"/Users/mikel/Workshop/src/delectus/webapp/delectus-server/")

(defmethod %server-pathname ((which (eql :public)))
  (merge-pathnames "public/" (%server-pathname :root)))

(defmethod %server-pathname ((which (eql :public/css)))
  (merge-pathnames "public/css/" (%server-pathname :root)))

(defmethod %server-pathname ((which (eql :public/js)))
  (merge-pathnames "public/js/" (%server-pathname :root)))

(defmethod %server-pathname ((which (eql :public/images)))
  (merge-pathnames "public/images/" (%server-pathname :root)))

(defmethod %server-pathname ((which (eql :public/templates)))
  (merge-pathnames "public/templates/" (%server-pathname :root)))

