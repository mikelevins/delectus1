;;; utilities for talking to our CouchDB server
;;; this code works only when run from the Delectus Couch server

;;; (ql:quickload :clouchdb)
;;; (ql:quickload :fare-csv)
(defpackage :delectus (:use :cl :clouchdb :parenscript))
(in-package :delectus)

;;; ---------------------------------------------------------
;;; tools for maintaining user-specified order
;;; ---------------------------------------------------------

;;; maximum safe integer in Javascript
(defparameter *maximum-order* 9007199254740991)
;;; minimum safe integer in Javascript
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

;;; ---------------------------------------------------------
;;; construct user-database names
;;; ---------------------------------------------------------

(defun string->hexstring (str)
  (string-downcase
   (with-output-to-string (out)
     (format out "~{~X~}"
             (map 'list #'char-code str)))))

(defun username->dbname (username)
  (let ((hex (string->hexstring username)))
    (format nil "userdb-~A" hex)))

;;; ---------------------------------------------------------
;;; talking to CouchDB
;;; ---------------------------------------------------------

(defun init-couch (username password)
  (set-connection :user username
                  :password password
                  :host "localhost"
                  :protocol "http"
                  :port "5984"
                  :name (username->dbname "delectus")))

;;; read the Movies.csv file:
;;; (defparameter $movies-path "/home/mikel/Workshop/src/delectus/test-data/Movies.csv")
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
