;;;; delectus.lisp

(in-package #:delectus)

(defun make-uuid-string ()
  (string-downcase (format nil "~A" (uuid:make-v4-uuid ))))

(defparameter $site-uuid (make-uuid-string))

(defun read-csv (path)
  (fare-csv:with-rfc4180-csv-syntax ()
    (let* ((fare-csv:*separator* #\,))
      (fare-csv:read-csv-file path))))

;;; (defparameter $csvpath "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (time (defparameter $data (read-csv $csvpath)))
;;; (length $data)
;;; (elt $data 0)
;;; (elt $data 43191)

(defun op-create-list (listid list-name)
  (assert list-name () "list-name parameter is required")
  (let ((id (or listid (make-uuid-string))))
    `("create-list" ,id nil nil ,list-name)))

;;; turn a list of csv data lists into a delectus op log
(defun csv-data->op-log (list-name data)
  (let* ((column-labels (first data))
         (row-data (rest data))
         (log (make-array 128
                          :initial-element nil
                          :adjustable t
                          :fill-pointer 0))
         (create-op (op-create-list nil list-name))
         (list-id (second create-op)))
    (vector-push-extend op log 128)
    (loop for lbl in column-labels
       do (vector-push-extend  (op-create-column list-id nil nil nil) log 128))
    log))

;;; (time (defparameter $log (csv-data->op-log "zipcodes" $data)))
;;; (length $log)
;;; $log
