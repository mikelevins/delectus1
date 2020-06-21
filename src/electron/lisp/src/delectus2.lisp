;;;; delectus2.lisp

(in-package #:delectus-ui)

(defparameter *server* nil)

(defun start-server (port)
  (setf *server*
        (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*))

(hunchentoot:define-easy-handler (landing :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (with-html-output-to-string (out nil :prologue t)
    (:html
     (:head
      (:title "Delectus 2")
      (:link :rel "preconnect" :href "https://cdn.jsdelivr.net")
      (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@native-elements/core@1/dist/native-elements.css"))
     (:body :style "background-color: #F0EBCB"
      (:script :src "https://unpkg.com/htmx.org@0.0.4")
      (:h1 "Delectus 2")
      (:div
       (:form
        (:input :type "text" :name "msg")
        (:br)
        (:button :class "btn"
                 :hx-post "/btnclick"
                 :hx-target "#response"
                 "Send message")))
      (:div :id "response")))
    (values)))


(hunchentoot:define-easy-handler (btnclick :uri "/btnclick") (msg)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Received: ~A" msg))

(defun stop-server ()
  (hunchentoot:stop *server*)
  (setf *server* nil))

;;; (start-server 9876)
;;; (stop-server)

(in-package #:cl-user)

(defun main (&optional (port 9876))
  (delectus-ui::start-server port)
  (sb-impl::toplevel-repl nil))

