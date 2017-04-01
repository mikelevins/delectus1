;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.asd
;;;; Project:       Delectus 2
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(asdf:defsystem #:delectus
    :description "Delectus: a list manager"
    :author "mikel evins <mevins@me.com>"
    :license "Apache 2.0" :serial t
    :depends-on (:dbd-sqlite3)
    :components ((:module "src" :serial t
                          :components ((:file "package")
                                       (:file "version")
                                       (:file "legacy")))))


;;; (asdf:load-system :delectus)

#|

 (dbi:with-connection (conn :sqlite3 :database-name "/Users/mikel/Desktop/test.db")
   (let* ((query (dbi:prepare conn "SELECT * FROM People"))
          (result (dbi:execute query)))
     (loop for row = (dbi:fetch result)
           while row
           do (format t "~A~%" row))))

|#