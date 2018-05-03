;;;; delectus-server.asd

(asdf:defsystem #:delectus-server
  :description "Describe delectus-server here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:clouchdb :hunchentoot :easy-routes)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "database")
                                     (:file "delectus-server")))))

;;; (asdf:load-system :delectus-server)
