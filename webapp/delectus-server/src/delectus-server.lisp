;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *delectus-server* nil)
(defparameter *delectus-dispatcher* nil)

(defun start-server (&key (port (server-port)))
  (setq cl-who:*attribute-quote-char* #\") ; to make it easier to use strings in html attributes
  (let* ((acceptor (make-instance 'hunchentoot:acceptor
                                  :port port
                                  :document-root (server-pathname :public)))
         (dispatcher (snooze:make-hunchentoot-app)))
    ;; 1. setf (acceptor-document-root acceptor) to the public directory
    (setf (hunchentoot:acceptor-document-root acceptor)
          (server-pathname :public))
    ;; 2. put the dispatcher onto hunchentoot:*dispatch-table*
    (setf hunchentoot:*dispatch-table*
          (list dispatcher 'hunchentoot:dispatch-easy-handlers))
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

