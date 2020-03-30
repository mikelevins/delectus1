;;;; delectus.asd

(asdf:defsystem #:delectus
  :description "Describe delectus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:fset :fare-csv :uuid :sqlite)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "read-csv")
                                     (:file "sqlite")
                                     (:file "delectus")))))

;;; (asdf:load-system :delectus)
