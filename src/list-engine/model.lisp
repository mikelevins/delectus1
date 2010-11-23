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
;;; model is the in-RAM data model for delectus. A Delectus document is
;;; conceptually a list whose elements are lists of fields. A field is
;;; a container for either a number or a string.

;;; ---------------------------------------------------------------------
;;; stretchy-vectors
;;; ---------------------------------------------------------------------
;;; a vector that can grow as needed

(defparameter $stretchy-vector-margin 8)

(defclass stretchy-vector ()
  ((elements :accessor elements)))

(defmethod initialize-instance ((v stretchy-vector) &rest initargs 
                                &key (initial-elements nil) (element-type t)
                                &allow-other-keys)
  (assert (every #'(lambda (e)(or (null e)(typep e element-type))) initial-elements)
          ()
          "the stretchy-vector requires elements of type ~S" element-type)
  (let ((v (call-next-method))
        (element-count (length initial-elements)))
    (with-slots (elements) v
      (setf elements 
            (make-array element-count
                        :element-type element-type
                        :initial-contents initial-elements
                        :adjustable t :fill-pointer element-count))
      (adjust-array elements (+ element-count $stretchy-vector-margin)))
    v))

(defmethod print-object ((v stretchy-vector)(s stream))
  (print-unreadable-object (v s :type t)
    (format s "~{~A ~}" (as 'list (elements v)))))

(defun make-stretchy-vector (vals &key (element-type t))
  (make-instance 'stretchy-vector :initial-elements vals :element-type element-type))

(defmethod count-elements ((v stretchy-vector))
  (length (elements v)))

(defmethod add-element! ((v stretchy-vector) val)
  (vector-push-extend val (elements v)))

(defmethod element ((v stretchy-vector)(index integer))
  (elt (elements v) index))

(defmethod %set-element! ((v stretchy-vector)(index integer) val)
  (setf (elt (elements v) index) val))

(defsetf element %set-element!)

(defmethod elements ((l list)) (as 'vector l))
(defmethod elements ((v vector)) v)

;;; ---------------------------------------------------------------------
;;; row
;;; ---------------------------------------------------------------------
;;; a sequence of fields

(defclass row (stretchy-vector)
  ((index :accessor index :initarg :index :initform 0)
   (deleted? :accessor deleted? :initarg :deleted :initform nil))
  (:default-initargs :element-type 'string))

(defun make-row (vals)
  (assert (every #'(lambda (e)(or (null e) (stringp e))) vals)
          ()
          "rows can contain only strings or nil")
  (make-instance 'row :initial-elements vals))

(defmethod ensure-row ((row row)) row)
(defmethod ensure-row ((vals list)) 
  (make-row vals))


(defmethod row? (x) nil)
(defmethod row? ((r row)) t)


;;; ---------------------------------------------------------------------
;;; column
;;; ---------------------------------------------------------------------
;;; a label for a p[osition in a row

(defclass column ()
  ((label :accessor label :initarg :label :initform nil)
   (index :accessor index :initarg :index :initform 0)
   (deleted? :accessor deleted? :initarg :deleted :initform nil)))

(defun make-column (label &key (index 0)(deleted nil))
  (make-instance 'column :label label :index index :deleted deleted))

(defmethod ensure-column ((col column)) col)
(defmethod ensure-column ((label string)) 
  (make-column label))

(defmethod column? (x) nil)
(defmethod column? ((col column)) t)

(defmethod print-object ((col column)(s stream))
  (print-unreadable-object (col s :type t)
    (format s "~A" (label col))))

;;; ---------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------
;;; a sequence of rows of length L, and a sequence of columns, also of length L

(defclass model ()
  ((columns :accessor columns :initarg :columns :initform (make-stretchy-vector nil :element-type 'column))
   (rows :accessor rows :initarg :rows :initform (make-stretchy-vector nil :element-type 'array))))

(defmethod index-columns ((m model))
  (loop
     for i from 0 upto (1- (length (elements (columns m))))
     and col across (elements (columns m))
     do (setf (index col) i)))

(defmethod index-rows ((m model))
  (loop
     for i from 0 upto (1- (length (elements (rows m))))
     and row across (elements (rows m))
     do (setf (index row) i)))

(defmethod initialize-instance :after ((m model) &rest initargs
                                       &key columns rows
                                       &allow-other-keys)
  (assert (every #'column? (elements columns))() "the columns argument must contain only COLUMN objects")
  (assert (every #'row? (elements rows))() "the rows argument must contain only ROW objects")
  (index-columns m)
  (index-rows m))

(defun make-model (&key columns rows)
  (make-instance 'model
                 :columns (make-stretchy-vector (mapcar #'ensure-column columns) :element-type 'column)
                 :rows (make-stretchy-vector (mapcar #'ensure-row rows) :element-type 'row)))

(defmethod print-object ((m model)(s stream))
  (print-unreadable-object (m s :type t)
    (format s "~{~A ~}" (as 'list (elements (columns m))))))

(defmethod find-column ((m model)(column-label string))
  (find column-label (elements (columns m)) :test (lambda (a b)(equalp a (label b)))))

(defmethod value-at ((m model)(column-label string)(row-index integer))
  (element (element (rows m) row-index)
           (index (find-column m column-label))))

(defmethod put-value-at! ((m model)(column-label string)(row-index integer) val)
  (setf (element (element (rows m) row-index)
           (index (find-column m column-label)))
        val))

(defmethod count-columns ((m model))
  (count-elements (columns m)))

(defmethod count-rows ((m model))
  (count-elements (rows m)))

(defmethod add-row! ((m model))
  (add-element! (rows m)
                (make-row (seq:repeat (count-columns m) nil))))

(defmethod add-column! ((m model)(label string))
  (if (find label (elements (columns m)) :test (lambda (a b)(equalp a (label b))))
      (error "Column label \"~A\" is already in use" label)
      (progn
        (add-element! (columns m)
                      (make-column label :index (count-columns m)))
        (loop for row across (elements (rows m))
           do (add-element! row nil)))))


;;; (setq $m (make-model :columns '("Name" "Rank" "Serial Number") :rows '(("Fred" "Husband" "3")("Barney" "Lackey" "2")("Wilma" "Boss" "1"))))
;;; (value-at $m "Rank" 2)
;;; (put-value-at! $m "Rank" 2 "Poobah")
;;; (add-column! $m "Name")
;;; (add-column! $m "Shape")
;;; (columns $m)
;;; (rows $m)
;;; (add-row! $m)
