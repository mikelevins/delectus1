;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *delectus-server* nil)

(defun start-server (&key (port (server-port)))
  (let* ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :port port))
         (dispatcher (snooze:make-hunchentoot-app)))
    ;; 1. setf (acceptor-document-root acceptor) to the public directory
    (setf (hunchentoot:acceptor-document-root acceptor)
          (server-pathname :public))
    ;; 2. push the dispatcher onto hunchentoot:*dispatch-table*
    (push dispatcher hunchentoot:*dispatch-table*)
    ;; 3. hunchentoot:start acceptor
    (hunchentoot:start acceptor)
    ;; 4. return the acceptor
    acceptor))

(defun stop-server (server)
  (hunchentoot:stop server))

;;; (defparameter *delectus-server* (start-server :port (server-port)))
;;; (stop-server *delectus-server*)

