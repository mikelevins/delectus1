;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *delectus-server* nil)

(defun start-server (&key (port 8080))
  (let* ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :port port))
         (dispatcher (snooze:make-hunchentoot-app)))
    ;; 1. setf (acceptor-document-root acceptor) to the public directory
    ;; 2. pushnew the dispatcher onto hunchentoot:*dispatch-table*
    ;; 3. stuff acceptor into *delectus-server*
    ;; 4. hunchentoot:start acceptor
    acceptor))

(defun stop-server ()
  (hunchentoot:stop *delectus-server*))

;;; (defparameter *delectus-server* (start-server))
;;; (stop-server *delectus-server*)

