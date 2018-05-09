;;;; delectus-server.lisp

(in-package #:delectus-server)

(defparameter *server* nil)

(easy-routes:defroute root ("/") ()
  "Hello from Delectus!")

(easy-routes:defroute name ("/name/:name") ()
  (format nil "Hello, ~A" name))

(defun start-server ()
  (setf *server*
        (hunchentoot:start (make-instance 'easy-routes:routes-acceptor
                                          :port 8080))))

(defun stop-server ()
  (hunchentoot:stop *server*))

;;; (start-server)
;;; (stop-server)
