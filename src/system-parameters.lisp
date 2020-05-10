;;;; ***********************************************************************
;;;;
;;;; Name:          system-parameters.lisp
;;;; Project:       delectus 2
;;;; Purpose:       set up pathnames and other parameters
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; TODO: add logic for initializing parameters appropriately in a delivered app

(defparameter *delectus-root-pathname* (asdf:system-relative-pathname :delectus "./"))

;;; query parameters
;;; ---------------------------------------------------------------------

(defparameter *default-result-items-per-page* 25)

;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *listnames-table-name* "listnames")
(defparameter *columns-table-name* "columns")
(defparameter *items-table-name* "items")

;;; column names
;;; ------------

(defparameter *delectus-columns* [ :|listid| :|fileid| :|format| :|modified| :|next_revision| ])
(defparameter *listnames-metadata-columns* [:|opid| :|revision| :|timestamp| :|name|])
(defparameter *comments-metadata-columns* [:|opid| :|revision| :|timestamp| :|comment|])
(defparameter *columns-metadata-columns* [:|opid| :|revision| :|timestamp|])
(defparameter *items-metadata-columns* [:|opid| :|revision| :|timestamp| :|itemid| :|deleted|])

(defparameter *listid-column-name* "listid")
(defparameter *format-column-name* "format")
(defparameter *modified-column-name* "modified")
(defparameter *next-revision-column-name* "next_revision")
(defparameter *opid-column-name* "opid")
(defparameter *revision-column-name* "revision")
(defparameter *timestamp-column-name* "timestamp")
(defparameter *name-column-name* "name")
(defparameter *comment-column-name* "comment")
(defparameter *itemid-column-name* "itemid")
(defparameter *deleted-column-name* "deleted")

;;; column parameters
;;; -----------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *default-initial-column-order* *minimum-column-order*)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

