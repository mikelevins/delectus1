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
;;; row
;;; ---------------------------------------------------------------------
;;; a sequence of fields

(defclass row ()
  ((elements :reader elements :initarg :elements)
   (deleted? :accessor deleted? :initform nil)))

(defmethod initialize-instance :after ((r row) &rest initargs &key &allow-other-keys)
  (setf (slot-value r 'elements)
        (apply #'seq:make (mapcar #'ensure-field (elements r)))))

(defun make-row (vals)
  (make-instance 'row :elements vals))

(defmethod ensure-row (v)
  (error "Can't make a row from value ~S" x))

(defmethod ensure-row ((vals list))
  (make-row vals))

(defmethod ensure-row ((r row)) r)

(defmethod print-object ((r row)(s stream))
  (print-unreadable-object (r s :type t)
    (format s "~{~A ~}" (as 'list (elements r)))))

(defmethod element ((r row)(index integer))
  (val (seq:element (elements r) index)))

(defmethod %set-element! ((r row)(index integer) val)
  (setf (val (seq:element (elements r) index))
        val))

(defsetf element %set-element!)

;;; (setq $r (make-row (list "zero" 1 2 "three" "4" "5" "six")))
;;; (element $r 1)
;;; (setf (element $r 1) "12.34")
;;; $r

;;; ---------------------------------------------------------------------
;;; column
;;; ---------------------------------------------------------------------
;;; a label for a position in a row

(defclass column ()
  ((label :reader label :initarg :label)
   (index :accessor index :initarg :index)
   (deleted? :accessor deleted? :initform nil)))

(defmethod make-column ((label string))
  (make-instance 'column :label label))

(defmethod ensure-column (x)
  (error "Can't make a column from value ~S" x))

(defmethod ensure-column ((c column)) c)

(defmethod ensure-column ((c string)) (make-column c))

(defmethod column-label (x)
  (error "Can't get a column label from value ~S" x))

(defmethod column-label ((c column)) (label c))

(defmethod column-label ((c string)) c)

(defun column-equal (x y) (equalp (column-label x)(column-label y)))

(defmethod print-object ((c column)(s stream))
  (print-unreadable-object (c s :type t)
    (format s "~A" (label c))))

;;; (make-column "Name")

;;; ---------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------
;;; a sequence of rows of length L, and a sequence of columns, also of length L

(defmethod ensure-no-duplicates ((cols list))
  (if (null cols)
      nil
      (if (member (car cols)
                  (cdr cols)
                  :test #'column-equal)
          (error "Duplicate column labels in ~S" cols)
          (cons (car cols)
                (ensure-no-duplicates (cdr cols))))))

(defmethod ensure-columns ((cols list))
  (if (null cols)
      (seq:make)
      (apply #'seq:make (mapcar #'ensure-column 
                                (ensure-no-duplicates cols)))))

(defmethod ensure-rows ((rows list))
  (if (null rows)
      (seq:make)
      (as 'fset:seq (seq:image #'ensure-row rows))))

(defclass model ()
  ((columns :accessor columns :initarg :columns :initform (seq:make))
   (rows :accessor rows :initarg :rows :initform (seq:make))
   (changed? :accessor changed? :initform t)))

(defmethod initialize-instance ((m model) &rest initargs 
                                &key columns rows
                                &allow-other-keys)
  (let ((m (call-next-method))
        (columns (ensure-columns columns))
        (rows (ensure-rows rows)))
    (dotimes (i (seq:length columns))
      (setf (index (seq:element columns i)) i))
    (setf (columns m)  columns)
    (setf (rows m)  rows)
    m))

(defun make-model (&key columns rows)
  (make-instance 'model :columns columns :rows rows))

(defmethod print-object ((m model)(s stream))
  (print-unreadable-object (m s :type t)
    (format s "~{~A ~}" (as 'list (columns m)))))

(defmethod find-column ((m model)(column-label string))
  (seq:find (lambda (col)(equalp (label col) column-label)) (columns m)))

(defmethod value-at ((m model)(column-label string)(row-index integer))
  (element (seq:element (rows m) row-index)
           (index (find-column m column-label))))

(defmethod put-value-at! ((m model)(column-label string)(row-index integer) val)
  (setf (element (seq:element (rows m) row-index)
                 (index (find-column m column-label)))
        val)
  (setf (changed? m) t)
  val)


;;; (setq $m (make-model :columns '("Name" "Rank" "Serial Number") :rows '(("Fred" "Husband" 3)("Barney" "Lackey" 2)("Wilma" "Boss" 1))))
;;; (value-at $m "Rank" 2)
;;; (put-value-at! $m "Rank" 2 "Poobah")
