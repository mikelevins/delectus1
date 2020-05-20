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

;;; root of the Delectus app's file tree
;;; ---------------------------------------------------------------------

(defparameter *delectus-root-pathname* (asdf:system-relative-pathname :delectus ""))

;;; query parameters
;;; ---------------------------------------------------------------------

(defparameter *default-result-items-per-page* 25)

;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *listnames-table-name* "listnames")
(defparameter *comments-table-name* "comments")
(defparameter *columns-table-name* "columns")
(defparameter *items-table-name* "items")

;;; column names
;;; ------------

(defparameter *delectus-columns* [ :|listid| :|format| :|created| :|modified| :|next_revision| :|next_itemid|])
(defparameter *listnames-metadata-columns* [:|revision| :|origin| :|timestamp| :|name|])
(defparameter *comments-metadata-columns* [:|revision| :|origin| :|timestamp| :|comment|])
(defparameter *columns-metadata-columns* [:|revision| :|origin| :|timestamp|])
(defparameter *items-metadata-columns* [:|revision| :|origin| :|timestamp| :|itemid| :|deleted|])

(defparameter *listid-column-name* "listid")
(defparameter *format-column-name* "format")
(defparameter *created-column-name* "created")
(defparameter *modified-column-name* "modified")
(defparameter *next-revision-column-name* "next_revision")
(defparameter *next-itemid-column-name* "next_itemid")
(defparameter *revision-column-name* "revision")
(defparameter *origin-column-name* "origin")
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

