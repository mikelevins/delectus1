;;; delectus CouchDB utilities

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :delectus
  :version "0.1"
  :serial t
  :depends-on (:fare-csv :clouchdb :parenscript)
  :components
  ((:file "package")
   (:file "users")))

;;; (asdf:load-system :delectus)
