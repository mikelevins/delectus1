;;;; delectus.asd

(asdf:defsystem :delectus-sync
  :description "delectus sync engine"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "2.0.1"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components (:file "package")
                        (:file "sync"))))

;;; (asdf:load-system :delectus-sync)

