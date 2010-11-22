;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          model.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; map-engine/model is an experiment in using maps instead of sequences,
;;; for the sake of the increased flexibility they provide

;;; ---------------------------------------------------------------------
;;; field
;;; ---------------------------------------------------------------------
;;; a container for a Delectus value

(defclass field ()
  ((val :accessor val :initform nil :initarg :val)))

(defun make-field (v)
  (make-instance 'field :val v))

(defmethod ensure-field (v) (make-field v))
(defmethod ensure-field ((v field)) v)

(defmethod print-object ((f field)(s stream))
  (print-unreadable-object (f s :type t)
    (format s "~A" (val f))))

;;; (setq $f (make-field nil))
;;; (val $f)
;;; (setq $f (make-field "1001.2"))
;;; (val $f)
;;; (setf (val $f) "12")
;;; (val $f)

;;; ---------------------------------------------------------------------
;;; columns
;;; ---------------------------------------------------------------------
;;; in the map engine, a column is a top-level key. its value is another
;;; map, from indexes to fields

(defclass column ()
  ((label :reader label :initarg :label)
   (deleted? :accessor deleted? :initarg :deleted :initform nil)
   (rows :accessor rows :initarg :rows :initform {})
   (max-row-index :accessor max-row-index :initarg :max-row-index :initform 0)))

(defmethod print-object ((c column)(s stream))
  (print-unreadable-object (c s :type t)
    (format s "~A (~A rows)" (label c)(seq:length (rows c)))))

#|
(defun make-column (label vals)
  (let ((fields (seq:image #'make-field vals))
        (data {})
        (max-index 0))
    (dotimes (i (seq:length fields))
      (setf data (map:associate data i (seq:element fields i)))
      (setf max-index i))
    (make-instance 'column :label label :rows data :max-row-index max-index)))
|#

(defun make-column (label)(make-instance 'column :label label))

;;; (setq $c (make-column "Name" '("Fred" "Barney")))

;;; ---------------------------------------------------------------------
;;; rows
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------

(defclass model ()
  ((index->column-map :accessor index->column-map :initarg :index->column-map :initform {})
   (column->index-map :accessor column->index-map :initarg :column->index-map :initform {})
   (label->column-map :accessor label->column-map :initarg :label->column-map :initform {})
   (row-count :accessor row-count :initarg :row-count :initform 0)
   (column-count :accessor column-count :initarg :column-count :initform 0)))

(defun make-model (&key columns rows)
  (let* ((cols (mapcar #'make-column columns))
         (col-count (length cols))
         (last-col-index (1- col-count))
         (row-index 0)
         (index->col {})
         (col->index {})
         (lbl->col {}))
    (loop
       for i from 0 upto last-col-index
       and lbl in columns
       and col in cols
       do (setf index->col (fset:with index->col i col)
                col->index (fset:with col->index col i )
                lbl->col (fset:with lbl->col lbl col)))
    (dolist (row rows)
      (loop
         for i from 0 upto last-col-index
         and v in row
         and col in cols
         do (setf (rows col)(fset:with (rows col) row-index (make-field v))))
      (incf row-index))
    (make-instance 'model 
                   :row-count (length rows)
                   :column-count col-count
                   :index->column-map index->col
                   :column->index-map col->index
                   :label->column-map lbl->col)))

(defmethod columns ((m model))
  (map:vals (index->column-map m)))

(defmethod column ((m model)(index integer))
  (map:get (index->column-map m) index :default nil))

(defmethod column ((m model)(label string))
  (map:get (label->column-map m) label :default nil))

(defmethod column-index ((m model)(col column))
  (map:get (column->index-map m) col :default nil))

(defmethod rows ((m model))
  (loop for j from 0 upto (1- (count-rows m))
       collect (loop for i from 0 upto (1- (count-columns m))
                  collect (value-at m i j))))


(defmethod count-columns ((m model))(column-count m))
(defmethod count-rows ((m model))(row-count m))

;;; ---------------------------------------------------------------------
;;; adding a column
;;; ---------------------------------------------------------------------

(defmethod add-column! ((m model)(column-label string))
  (if (column m column-label)
      (error "Column '~A' exists" column-label)
      (let* ((index (count-columns m))
             (col (make-instance 'column 
                                 :label column-label
                                 :rows (map:zipmap (as 'list (seq:range 0 (count-rows m)))
                                                   (as 'list (seq:image #'make-field
                                                                        (seq:repeat (count-rows m) nil)))))))
        (setf (index->column-map m)
              (map:associate (index->column-map m) index col))
        (setf (column->index-map m)
              (map:associate (column->index-map m) col index))
        (setf (label->column-map m)
              (map:associate (label->column-map m) column-label col))
        (incf (column-count m)))))

(defmethod add-row! ((m model))
  (dotimes (i (count-columns m))
    (let ((col (column m i)))
      (setf (rows col)
            (seq:add-last (rows col)(make-field nil))))))


;;; ---------------------------------------------------------------------
;;; getting and setting items
;;; ---------------------------------------------------------------------

(defmethod value-at ((m model)(column-label string)(row-index integer))
  (val (map:get (rows (column m column-label)) row-index)))

(defmethod value-at ((m model)(column-index integer)(row-index integer))
  (val (map:get (rows (column m column-index)) row-index)))

(defmethod put-value-at! ((m model)(column-label string)(row-index integer) val)
  (let ((col (column m column-label)))
    (if col
        (if (<= row-index (max-row-index col))
            (let ((field (map:get (rows col) row-index)))
              (setf (val field) val)
              val)
            (error "No such row: '~A'" row-index))
        (error "No such column: '~A'" column-label))))



#|
(setq $m 
      (make-model :columns '("First Name" "Last Name" "Home town")
                  :rows '(("Fred" "Flintstone" "Bedrock")
                          ("Barney" "Rubble" "Bedrock")
                          ("Wilma" "Flintstone" "Bedrock"))))
(columns $m)
(label->column-map $m)
(describe (column $m "Home town"))
(column $m "Best Friend")
(value-at $m "First Name" 0)
(rows (column $m 0))
(rows $m)
(add-column! $m "Best Friend")
(put-value-at! $m "Best Friend" 0 "Barney")
(put-value-at! $m "Best Friend" 1 "Fred")
|#

