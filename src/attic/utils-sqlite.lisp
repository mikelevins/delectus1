;;;; ***********************************************************************
;;;;
;;;; Name:          utils-sqlite.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       sqlite utility functions 
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.sqlite)

 #+linux(defparameter $home "/home/")
 #+darwin(defparameter $home "/Users/")

;;; a couple of test files to work with
(defparameter $moviesdb (merge-pathnames "mikel/Workshop/src/delectus/test-data/Movies.delectus2" $home))
(defparameter $redditdb (merge-pathnames
                         "mikel/Warehouse/Data/reddit/2017-10/segmented/sqlite/RC_2017-10_part_0000.sqlite"
                         $home))

;;; what tables are in a sqlite file?

;;; list-tables
(defun list-tables (path)
  (sqlite:with-open-database (db path)
    (mapcar #'car (sqlite:execute-to-list db "SELECT name FROM sqlite_master WHERE type = \"table\""))))

;;; (list-tables $moviesdb)
;;; (list-tables $redditdb)

;;; list-columns
(defun list-columns (path table-name)
  (sqlite:with-open-database (db path)
    (sqlite:execute-to-list db (format nil "pragma table_info(~S)" table-name))))

;;; both sample databases happen to have "contents" tables
;;; (list-columns $moviesdb "contents")
;;; (time (list-columns $redditdb "contents"))

;;; list-column-names
;;; extract the names of the columns from table column info
(defun list-column-names (path table-name)
  (mapcar #'(lambda (info)(second info))
          (list-columns path table-name)))

;;; (list-column-names $moviesdb "contents")
;;; (time (list-column-names $redditdb "contents"))

;;; count-rows
;;; return the number of rows in the named table
(defun count-rows (path table-name)
  (sqlite:with-open-database (db path)
    (sqlite:execute-single db (format nil "select count(*) from ~A" table-name))))

;;; (time (count-rows $moviesdb "contents"))
;;; (time (count-rows $redditdb "contents"))

;;; get-rows
;;; return the number of rows in the named table
(defun get-rows (path table-name &key (from 0) (count nil))
  (sqlite:with-open-database (db path)
    (let ((count (or count
                     (count-rows path table-name))))
      (sqlite:execute-to-list db (format nil "select * from ~a limit ~d,~d" table-name from count)))))

;;; (time (length (get-rows $redditdb "contents")))
;;; (time (get-rows $redditdb "contents" :from 0 :count 4))
;;; (time (get-rows $redditdb "contents" :from 90000 :count 4))

;;; get-row
;;; return a specified row in the named table
(defun get-row (path table-name index)
  (sqlite:with-open-database (db path)
    (sqlite:execute-to-list db (format nil "select * from ~a limit ~d,~d" table-name index 1))))

;;; (time (get-row $redditdb "contents" 0))
;;; (time (get-row $redditdb "contents" 1000))
