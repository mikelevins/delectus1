;;;; delectus.asd

(asdf:defsystem :delectus-engine
  :description "delectus data and sync engine"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "2.0.1"
  :serial t
  :depends-on (:sqlite :local-time :cl-conspack :sha3)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "identity")
                                     (:file "messages")
                                     (:file "state")
                                     (:file "changes")
                                     ))))

;;; (asdf:load-system :delectus-engine)
