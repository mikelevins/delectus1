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

;;; an identity generated once per Delectus launch
(defparameter *process-identity* nil)

(defun process-identity ()
  (unless *process-identity*
    (setf *process-identity* (makeid)))
  *process-identity*)

;;; list-file parameters
;;; ---------------------------------------------------------------------

;;; table names
;;; -----------

(defparameter *delectus-table-name* "delectus")
(defparameter *syncs-table-name* "syncs")
(defparameter *listnames-table-name* "listnames")
(defparameter *columns-table-name* "columns")
(defparameter *items-table-name* "items")

;;; index names
;;; -----------

(defparameter *item-opid-origin-index-name* "idx_item_opid_origin")

;;; column names
;;; ------------

(defparameter *common-metadata-columns* ["origin" "timestamp"])
(defparameter *sync-metadata-columns* (append *common-metadata-columns* ["peer" "file"]))
(defparameter *listname-metadata-columns* (append *common-metadata-columns* ["name"]))
(defparameter *columns-metadata-columns* *common-metadata-columns*)
(defparameter *items-metadata-columns* (append *common-metadata-columns* ["item" "deleted"]))

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

