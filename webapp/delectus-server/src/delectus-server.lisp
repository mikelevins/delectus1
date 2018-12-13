;;;; delectus-server.lisp

(in-package #:delectus-server)

(defclass delectus-acceptor (hunchentoot:easy-acceptor) ())

(defparameter *delectus-dispatch-table*
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (server-pathname :public/images))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (server-pathname :public/js))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (server-pathname :public/css))
   (make-hunchentoot-app '((*home-resource* . homepage)))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((a delectus-acceptor) request)
  (let ((hunchentoot:*dispatch-table* *delectus-dispatch-table*))
    (call-next-method)))

(defparameter *delectus-server* nil)

(defun stop-server ()
  (when *delectus-server*
    (hunchentoot:stop *delectus-server*)
    (setq *delectus-server* nil)))

(defun start-server (&key (port (server-port)))
  (stop-server)
  (setq *delectus-server* (hunchentoot:start (make-instance 'delectus-acceptor :port port))))

;;; (start-server)
;;; (stop-server)
