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


;;; op parameters
;;; ---------------------------------------------------------------------

(defparameter *delectus-op-types* '("listname" "columns" "item" "sync"))

(defmethod optype? (thing)
  (declare (ignore thing))
  nil)

(defmethod optype? ((s string))
  (and (member s *delectus-op-types* :test #'equal)
       t))

(defmethod identity? (thing)
  (declare (ignore thing))
  nil)

(defmethod identity? ((s string))
  (and (= delectus::+delectus-identity-string-length+
          (length s))
       (eql #\I (elt s 0))))

(defmethod revision? (thing)
  (declare (ignore thing))
  nil)

(defmethod revision? ((thing integer))
  (>= thing 0))

(defmethod timestamp? (thing)
  (declare (ignore thing))
  nil)

(defmethod timestamp? ((thing string))
  (typep (local-time:parse-timestring thing)
         'local-time:timestamp))

(defmethod name? (thing)
  (declare (ignore thing))
  nil)

(defmethod name? ((thing string))
  (declare (ignore thing))
  t)

(defun deleted-flag? (thing)
  (and (member thing '(t nil))
       t))

;;; column parameters
;;; ---------------------------------------------------------------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; constructing columndata
;;; ---------------------------------------------------------------------

(defun columndata (&key id name type order sort title subtitle deleted)
  (list :|id| (or id :false)
        :|name| (or name :false)
        :|type| (or type :false)
        :|order| (or order :false)
        :|sort| (or sort :false)
        :|title| (or title :false)
        :|subtitle| (or subtitle :false)
        :|deleted| (or deleted :false)))

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

;;; (defparameter $op (listname :opid (delectus::makeid) :origin (delectus::makeid) :name "Movies" :revision 1 :timestamp (delectus::now-timestamp)))
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

(defmethod well-formed-op? (thing)
  (declare (ignore thing))
  nil)

(defmethod ensure-well-formed-op ((thing list))
  (assert (<= 9 (length thing))()
          "Expected 9 or more elements in an op, but found: ~S" thing)
  (let ((optype (op-field thing :optype))
        (opid (op-field thing :opid))
        (origin (op-field thing :opid))
        (revision (op-field thing :revision))
        (timestamp (op-field thing :timestamp))
        (item (op-field thing :item))
        (name (op-field thing :name))
        (deleted (op-field thing :deleted))
        (peer (op-field thing :peer)))
    (assert (optype? optype)() "Expected an optype but found ~S in ~S" optype thing)
    (assert (identity? opid)() "Expected an identity but found ~S in ~S" opid thing)
    (assert (identity? origin)() "Expected an identity but found ~S in ~S" origin thing)
    (assert (revision? revision)() "Expected a revision but found ~S in ~S" revision thing)
    (assert (timestamp? timestamp)() "Expected a timestamp but found ~S in ~S" timestamp thing)
    (assert (or (identity? item)(null item))() "Expected an identity or nil but found ~S in ~S" item thing)
    (assert (or (name? name)(null name))() "Expected a name or nil but found ~S in ~S" name thing)
    (assert (deleted-flag? deleted)() "Expected a deleted flag but found ~S in ~S" deleted thing)
    (assert (or (identity? peer)(null peer))() "Expected an identity or nil but found ~S in ~S" peer thing))
  thing)


