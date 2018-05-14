;;;; delectus.asd

(asdf:defsystem :delectus-engine
  :description "delectus data and sync engine"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "2.0.1"
  :serial t
  :depends-on (:cl-conspack)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "changes")
                                     ))))

;;; (asdf:load-system :delectus-engine)
