 ;;;; delectus.asd

(asdf:defsystem :engine
  ;; the system needs the same name as this file in order to load
  ;; properly in Lispworks 6.1 on Windows
  :description "delectus data and sync engine"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "2.0.1"
  :serial t
  :depends-on (:sqlite :local-time :cl-conspack :sha3)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "utils")
                                     (:file "identity")
                                     (:file "store")
                                     (:file "messages")
                                     (:file "changes")
                                     (:file "node")
                                     ))))

;;; Windows:
;;;
;;; load CFFI:
;;; (ql:quickload :cffi)
;;;
;;; tell CFFI where SQLite is:
;;; (push #P"C:\\sqlite\\32bit\\" cffi:*foreign-library-directories*)

;;; (asdf:load-system :engine)
