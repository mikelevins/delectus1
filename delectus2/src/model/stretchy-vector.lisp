;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stretchy-vector.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $stretchy-vector-default-size-increment 32)

(defun make-stretchy-vector (&key initial-size initial-contents)
  (if initial-contents
      (if initial-size
          (error "You must specify at most one of :initial-size or :initial-contents")
          (let ((size (length initial-contents)))
            (make-array size :initial-contents initial-contents :fill-pointer size :adjustable t)))
      (let ((size (or initial-size $stretchy-vector-default-size-increment)))
        (make-array size :fill-pointer 0 :adjustable t))))

(defmethod add-elt! ((vec vector) val)
  (vector-push-extend val vec $stretchy-vector-default-size-increment)
  vec)

(defmethod remove-elt! ((vec vector)(index integer))
  (let ((fp (fill-pointer vec)))
    (cond
      ((zerop fp) (error "Index ~A out of range" index))
      ((= index fp) vec)
      (t (progn
           (loop for i from index upto (- fp 2)
                do (setf (elt vec i) 
                         (elt vec (1+ i))))
           (setf (fill-pointer vec)(1- fp))
           vec)))))

;;; (setq $v (make-stretchy-vector))
;;; (add-elt! $v "a")
;;; (add-elt! $v "b")
;;; (add-elt! $v "c")
;;; (remove-elt! $v 0)
