;;; utilities for talking to our Cloudant server
;;; get username and password from the Cloudant dashboard

;;; (ql:quickload :clouchdb)
(defpackage :cloudant (:use :cl :clouchdb :parenscript))
(in-package :cloudant)

(defparameter *maximum-order* 9007199254740991)
(defparameter *minimum-order* -9007199254740991)

(defun choose-order (&optional
                       (lower-bound *minimum-order*)
                       (upper-bound *maximum-order*))
  (values (+ lower-bound (truncate (- upper-bound lower-bound) 2))
          lower-bound
          upper-bound))

(defun init-cloudant (username password)
  (set-connection :user username
                  :password password
                  :host "40c16efe-bc36-4c03-80ca-e5c3c97fa35f-bluemix.cloudant.com"
                  :protocol "https"
                  :port "443"
                  :name "delectus"))

