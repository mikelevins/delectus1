 ;;;; delectus.asd

(asdf:defsystem :engine
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
                                     (:file "sqlite")
                                     (:file "messages")
                                     (:file "changes")
                                     (:file "node")
                                     ))))

;;; (ql:quickload :cffi)
;;; (push #P"C:\\sqlite\\32bit\\" cffi:*foreign-library-directories*)
;;; (asdf:load-system :engine)

