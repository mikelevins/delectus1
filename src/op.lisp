;;;; ***********************************************************************
;;;;
;;;; Name:          op.lisp
;;;; Project:       delectus 2
;;;; Purpose:       constructing ops
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:op)

;;; op format:
;;;
;;; (optype opid origin revision timestamp item name deleted peer &rest columns)
;;;
;;; in lisp forms, COLUMNS parameters are collected into a &rest parameter
;;; in database tables, each element of COLUMNS gets a database column

;;; column parameters
;;; ---------------------------------------------------------------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; timestamps
;;; ---------------------------------------------------------------------

(defun now-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))

;;; (now-timestamp)

;;; constructing ops
;;; ---------------------------------------------------------------------

(defun op (&key optype opid origin revision timestamp item name deleted peer columns)
  `(,optype ,opid ,origin ,revision ,timestamp ,item ,name ,deleted ,peer ,@columns))

(defun op-field (op field-name)
  (ecase field-name
    (:optype (elt op 0))
    (:opid (elt op 1))
    (:origin (elt op 2))
    (:revision (elt op 3))
    (:timestamp (elt op 4))
    (:item (elt op 5))
    (:name (elt op 6))
    (:deleted (elt op 7))
    (:peer (elt op 8))
    (:columns (nthcdr 9 op))
    (t (error "Unrecognized op field name: ~S" field-name))))

(defun listname (&key opid origin name revision timestamp)
  (op :optype "listname" :opid opid :origin origin :name name :revision revision :timestamp timestamp))

;;; (defparameter $op (listname :opid (delectus::makeid) :origin (delectus::makeid) :name "Movies" :revision 1 :timestamp (now-timestamp)))
;;; (op-field $op :origin)

(defun columns (&key opid origin revision timestamp columns)
  (op :optype "columns" :opid opid :origin origin :revision revision :timestamp timestamp :columns columns))

;;; (defparameter $op (columns :opid (delectus::makeid) :origin (delectus::makeid) :revision 1 :timestamp (now-timestamp) :columns (list :col0 :col1 :col2)))
;;; (op-field $op :timestamp)

(defun item (&key opid origin revision timestamp item deleted fields)
  (op :optype "item" :opid opid :origin origin :revision revision :timestamp timestamp :item item :deleted deleted
      :columns fields))

;;; (defparameter $op (item :opid (delectus::makeid) :origin (delectus::makeid) :revision 1 :timestamp (now-timestamp) :item (delectus::makeid) :deleted t :fields (list :field0 :field1 :field2)))
;;; (op-field $op :timestamp)

(defun sync (&key opid origin revision timestamp peer)
  (op :optype "sync" :opid opid :origin origin :revision revision :timestamp timestamp :peer peer))

;;; (defparameter $op (sync :opid (delectus::makeid) :origin (delectus::makeid) :revision 1 :timestamp (now-timestamp) :peer (delectus::makeid)))
;;; (op-field $op :peer)


