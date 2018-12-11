;;;; delectus-server.asd

(asdf:defsystem #:delectus-server
  :description "The Delectus networked list-manager server"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :depends-on (:clack :snooze)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "delectus-server")))))

;;; (asdf:load-system :delectus-server)
