;;;; delectus-server.asd

(asdf:defsystem #:delectus-server
    :description "Web application server for Delectus 2.0"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2"
    :version "2.0.1"
    :depends-on (:clouchdb)
    :serial t
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "delectus-server")))))

;;; (asdf:load-system :delectus-server)
