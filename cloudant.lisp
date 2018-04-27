;;; utilities for talking to our Cloudant server
;;; get username and password from the Cloudant dashboard

;;; (ql:quickload :clouchdb)
;;; (ql:quickload :fare-csv)
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

(defun distribute-order (count
                         &optional
                           (lower-bound *minimum-order*)
                           (upper-bound *maximum-order*))
  (let ((interval (truncate (- upper-bound lower-bound)
                            (1+ count)))
        (result nil))
    (dotimes (i count (reverse result))
      (push (+ lower-bound (* interval (1+ i))) result))))

(defun init-cloudant (username password)
  (set-connection :user username
                  :password password
                  :host "40c16efe-bc36-4c03-80ca-e5c3c97fa35f-bluemix.cloudant.com"
                  :protocol "https"
                  :port "443"
                  :name "delectus"))

;;; read the Movies.csv file:
;;; (defparameter $movies-path "/home/mikel/Workspace/src/delectus/test-data/Movies.csv")
;;; (defparameter $movies (fare-csv:read-csv-file $movies-path))
;;; (defparameter $movie-columns (first $movies))

(defun make-delectus-column (column-name order)
  `(,column-name . ((:|order| . ,order)
                    (:|type| . "string"))))

(defun make-delectus-list (username column-names)
  (let* ((orders (distribute-order (length column-names))))
    `((:|username| . ,username)
      (:|type| . "List")
      (:|Fields| . ,(mapcar 'make-delectus-column column-names orders)))))

;;; (defparameter $movies-list (make-delectus-list "delectus" $movie-columns))
