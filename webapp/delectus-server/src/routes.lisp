;;;; routes.lisp

(in-package #:delectus-server)

(defun default-environment () nil)

(defroute homepage (:get "text/html")
          (process-template (path :templates "index.tmpl")
                            (default-environment)))
