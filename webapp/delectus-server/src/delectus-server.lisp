;;;; delectus-server.lisp

(in-package #:delectus-server)

(defun start-server ()
  (clack:clackup
   (lambda (env)
     (declare (ignore env))
     '(200 (:content-type "text/plain") ("Delectus 2 server")))))

(defun stop-server (server)
  (clack:stop server))

;;; (defparameter *delectus-server* (start-server))
;;; (stop-server *delectus-server*)

