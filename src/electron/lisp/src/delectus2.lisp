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
      (:link :rel "stylesheet" :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css"
             :integrity "sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk"
             :crossorigin "anonymous")
      (:style :type "text/css"
              "body { background: #F0EBCB"))
     (:body 
      (:script :src "https://unpkg.com/htmx.org@0.0.4")
      (:nav :class "navbar navbar-light bg-light"
            (:a :class "navbar-brand" :href "#" "Delectus")
            (:span :class "navbar-text" (:small (:em (str (format nil "Version ~A" delectus::+delectus-version+))))))
      (:div :class "container m-3"
            (:div  :class "my-2"
             (:form
              (:input :class "my-2" :type "text" :name "msg")
              (:br)
              (:button :class "btn btn-primary my-2"
                       :hx-post "/btnclick"
                       :hx-target "#response"
                       "Send message")))
            (:div :class "my-2" :id "response"))))
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

