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

;;; TODO: add a config subsystem and store this value in per-node
;;; configuration

(defparameter *origin* "I9447d300752b11ea8256d9dd744b5501")

;;; TODO: add logic for initializing parameters appropriately in a delivered app

(defparameter *delectus-root-pathname* (asdf:system-relative-pathname :delectus "./"))


;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *listdata-table-name* "listdata")

;;; index names
;;; -----------

(defparameter *item-opid-origin-index-name* "idx_item_opid_origin")

;;; column names
;;; ------------

(defparameter *metadata-column-names* '("optype" "opid" "origin" "timestamp" "peer" "file" "name" "item" "deleted"))
(defparameter *optype-column-name* "optype")
(defparameter *opid-column-name* "opid")
(defparameter *origin-column-name* "origin")
(defparameter *timestamp-column-name* "timestamp")
(defparameter *peer-column-name* "peer")
(defparameter *file-column-name* "file")
(defparameter *name-column-name* "name")
(defparameter *item-column-name* "item")
(defparameter *deleted-column-name* "deleted")

;;; column parameters
;;; -----------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *default-initial-column-order* *minimum-column-order*)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; op parameters
;;; -------------

(defparameter *sync-optype* "sync")
(defparameter *listname-optype* "listname")
(defparameter *columns-optype* "columns")
(defparameter *item-optype* "item")
