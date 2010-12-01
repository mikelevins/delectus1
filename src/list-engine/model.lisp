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

;;; =====================================================================
;;; model data

;;; ---------------------------------------------------------------------
;;; row
;;; ---------------------------------------------------------------------
;;; a stretchy-vector of data items

(defun make-row (vals)
  (assert (every #'(lambda (e)(or (null e) (stringp e))) vals)
          ()
          "rows can contain only strings or nil")
  (copy-tree vals))

;;; ---------------------------------------------------------------------
;;; rows
;;; ---------------------------------------------------------------------
;;; a stretchy-vector of rows

(defun make-rows (vals)
  (assert (every #'listp vals)()
          "MAKE-ROWS accepts only rows as input")
  (copy-tree vals))

;;; ---------------------------------------------------------------------
;;; columns
;;; ---------------------------------------------------------------------
;;; labels for positions in a row

(defun make-columns (vals)
  (assert (every #'(lambda (e)(or (null e) (stringp e))) vals)
          ()
          "a columns must be a string or nil")
  (copy-tree vals))

;;; ---------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------
;;; a sequence of rows of length L, and a sequence of columns, also of length L

(defclass model ()
  ((columns :accessor columns :initarg :columns :initform nil)
   (rows :accessor rows :initarg :rows :initform nil)))

(defun make-model (&key columns rows)
  (make-instance 'model
                 :columns (make-columns columns)
                 :rows (make-rows (mapcar #'make-row rows))))

(defmethod print-object ((m model)(s stream))
  (print-unreadable-object (m s :type t)
    (format s "~{~A ~}" (columns m))))

(defmethod find-column ((m model)(column-label string))
  (find column-label (columns m) :test #'equalp))

(defmethod column-index ((m model)(column-label string))
  (position column-label (columns m) :test #'equalp))

(defmethod value-at ((m model)(column-label string)(row-index integer))
  (elt (elt (rows m) row-index)
       (column-index m column-label)))

(defmethod put-value-at! ((m model)(column-label string)(row-index integer) val)
  (setf (elt (elt (rows m) row-index)
             (column-index m column-label))
        val))

(defmethod count-columns ((m model))
  (length (columns m)))

(defmethod count-rows ((m model))
  (length (rows m)))

(defmethod add-row! ((m model) &optional vals)
  (assert (or (null vals)
              (= (length vals)
                 (count-columns m)))
          ()
          "If values for the new row are supplied, then their count must equal the count of columns in the model")
  (let* ((vals (or vals (as 'list (seq:repeat (count-columns m) nil))))
         (new-row (make-row vals)))
    (setf (rows m)(cons new-row (rows m)))))

(defmethod add-column! ((m model)(label string))
  (if (find label (columns m) :test #'equalp)
      (error "Column label \"~A\" is already in use" label)
      (progn
        (setf (columns m) (append (columns m) (list label)))
        (loop for row in (rows m)
           do (setf (cdr (last row))
                    (list nil))))))

