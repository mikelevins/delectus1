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
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; stretchy-vectors
;;; ---------------------------------------------------------------------
;;; a vector that can grow as needed

(defparameter $stretchy-vector-margin 8)

(defun make-stretchy-vector (vals &key (element-type t))
  (adjust-array (make-array (length vals) :element-type element-type :initial-contents vals
                            :fill-pointer (length vals))
                (+ (length vals) $stretchy-vector-margin)))

(defmethod add-element! ((v array) val)
  (vector-push-extend val v))

;;; ---------------------------------------------------------------------
;;; row
;;; ---------------------------------------------------------------------
;;; a stretchy-vector of data items

(defun make-row (vals)
  (assert (every #'(lambda (e)(or (null e) (stringp e))) vals)
          ()
          "rows can contain only strings or nil")
  (make-stretchy-vector vals))

;;; ---------------------------------------------------------------------
;;; rows
;;; ---------------------------------------------------------------------
;;; a stretchy-vector of rows

(defun make-rows (vals)
  (assert (every #'(lambda (e)(or (null e) (vectorp e))) vals)
          ()
          "MAKE-ROWS accepts only rows as input")
  (make-stretchy-vector vals))


;;; ---------------------------------------------------------------------
;;; columns
;;; ---------------------------------------------------------------------
;;; labels for positions in a row

(defun make-columns (vals)
  (assert (every #'(lambda (e)(or (null e) (stringp e))) vals)
          ()
          "a columns must be a string or nil")
  (make-stretchy-vector vals))

;;; ---------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------
;;; a sequence of rows of length L, and a sequence of columns, also of length L

(defclass model ()
  ((columns :accessor columns :initarg :columns :initform (make-stretchy-vector nil))
   (rows :accessor rows :initarg :rows :initform (make-stretchy-vector nil))))

(defun make-model (&key columns rows)
  (make-instance 'model
                 :columns (make-stretchy-vector columns)
                 :rows (make-rows (mapcar #'make-row rows))))

(defmethod print-object ((m model)(s stream))
  (print-unreadable-object (m s :type t)
    (format s "~{~A ~}" (as 'list (columns m)))))

(defmethod find-column ((m model)(column-label string))
  (find column-label (columns m) :test #'equalp))

(defmethod column-index ((m model)(column-label string))
  (position column-label (columns m) :test #'equalp))

(defmethod value-at ((m model)(column-label string)(row-index integer))
  (aref (aref (rows m) row-index)
        (column-index m column-label)))

(defmethod put-value-at! ((m model)(column-label string)(row-index integer) val)
  (setf (aref (aref (rows m) row-index)
              (column-index m column-label))
        val))

(defmethod count-columns ((m model))
  (length (columns m)))

(defmethod count-rows ((m model))
  (length (rows m)))

(defmethod add-row! ((m model) &optional vals)
  (assert (or (null vals (= (length vals (count-columns m)))))()
          "If values for the new row are supplied, then their count must equal the count of columns in the model")
  (let ((vals (or vals (seq:repeat (count-columns m) nil))))
    (add-element! (rows m)
                  (make-row vals))))

(defmethod add-column! ((m model)(label string))
  (if (find label (columns m) :test #'equalp)
      (error "Column label \"~A\" is already in use" label)
      (progn
        (add-element! (columns m) label)
        (loop for row across (rows m)
           do (add-element! row nil)))))
