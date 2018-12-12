;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *delectus-server* nil)
(defparameter *delectus-dispatcher* nil)

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
    ;; 4. store the dispatcher and acceptor
    (setf *delectus-server* acceptor)
    (setf *delectus-dispatcher* dispatcher)
    ;; 5. return the acceptor and dispatcher
    (values acceptor dispatcher)))

(defun stop-server (server)
  (hunchentoot:stop server))

;;; (start-server :port (server-port))
;;; (stop-server *delectus-server*)

