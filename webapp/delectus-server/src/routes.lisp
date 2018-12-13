;;;; routes.lisp

(in-package #:delectus-server)

(defun default-environment ()
  (environment))

(defroute homepage (:get "text/html")
          (process-template (path :templates "index.tmpl")
                            (default-environment)))
