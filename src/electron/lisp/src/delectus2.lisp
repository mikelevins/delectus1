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
      (:script :type "text/javascript" (str (ps
                                              (var electron (require "electron"))
                                              (var remote (@ electron remote))
                                              (var dialog (@ remote dialog)))))
      (:script :type "text/javascript"
               (str (ps
                      (defun handle-open ()
                        (let ((chosen-path ((@ dialog |showOpenDialogSync|))))
                          (alert chosen-path))))))
      (:nav :class "navbar navbar-light bg-light"
            (:a :class "navbar-brand" :href "#" "Delectus")
            (:div :class "navbar-nav" (:button :class "btn" "New"))
            (:div :class "navbar-nav mr-auto" (:button :id "openBtn" :class "btn"
                                                       :onclick (ps (handle-open))
                                                       "Open"))
            (:span :class "navbar-text" (:small (:em (str (format nil "Version ~A" delectus::+delectus-version+))))))
      (:div :id "contents"
            :class "container m-3"
            (:div  :class "my-2"))))
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



