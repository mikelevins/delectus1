;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       conversion to and from csv format
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $csv-read-buffer-size 128)

(defmethod read-csv ((path pathname) &key (first-line-keys? t))
  (let ((reversed-lines nil))
    (csv-parser:do-csv-file ((fields num-fields) path)
      (push fields reversed-lines))
    (let* ((lines (reverse reversed-lines))
           (keys (if first-line-keys?
                     (mapcar #'item-key (car lines))
                     (mapcar #'item-key (take-labels (length (car lines))))))
           (items (mapcar (partial #'zip-item keys)
                          (if first-line-keys? (cdr lines) lines))))
      (make-instance 'delectus
                     :keys (make-stretchy-vector :initial-contents keys) 
                     :items (make-stretchy-vector :initial-contents items)))))

(defmethod read-csv ((path pathname) &key (first-line-keys? t))
  (let ((reversed-lines nil))
    (csv-parser:do-csv-file ((fields num-fields) path)
      (push fields reversed-lines))
    (let* ((lines (reverse reversed-lines))
           (keys (if first-line-keys?
                     (mapcar #'item-key (car lines))
                     (mapcar #'item-key (take-labels (length (car lines))))))
           (items (mapcar (partial #'zip-item keys)
                          (if first-line-keys? (cdr lines) lines))))
      (with-open-database (db ":memory:")
        (let ((create-statement (format nil 
                                        "create table delectus (~{~A text, ~})" keys)))
          (execute-non-query db create-statement)
          (loop for item in items
             do (let ((insert-statement (format nil
                                                "insert into users (~{~A, ~}) values (~{:~A, ~})"
                                                keys)))
                  )))))))

(defmethod read-csv ((path string) &key (first-line-keys? t))
  (read-csv (pathname path) :first-line-keys? first-line-keys?))


