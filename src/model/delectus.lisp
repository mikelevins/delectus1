;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.lisp
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
;;; "delectus" is from the Latin "delectum", meaning something
;;; chosen. A delectus is a set of selected items.

(defclass delectus ()
  ((items :reader items :initarg :items)))

(defmethod make-delectus ((vals list))
  (let ((item-count (length vals)))
    (make-instance 'delectus :items (make-array item-count :fill-pointer item-count
                                                :initial-contents vals :adjustable t))))

(defmethod make-delectus ((vals vector))
  (let ((item-count (length vals)))
    (make-instance 'delectus :items vals)))

(defmethod print-object ((del delectus)(s stream))
  (print-unreadable-object (del s :type t)
    (if (> (count del) 3)
        (format s "~{~A ~}... " (loop for i from 0 upto 2 collect (elt (items del) i)))
        (format s "~{~A ~}" (as 'list (items del))))))

(defmethod count ((del delectus))
  (length (items del)))

(defmethod item ((del delectus)(index integer))
  (elt (items del) index))

(defmethod add! ((del delectus) val)
  (vector-push-extend val (items del))
  val)

(defmethod remove! ((del delectus) (index integer))
  (let* ((items (items del))
         (item-count (count del))
         (new-items (make-array (1- item-count) :fill-pointer (1- item-count)
                                :initial-element nil :adjustable t)))
    (loop
       for i from 0 upto (1- index)
       do (setf (elt new-items i)
                (elt items i)))
    (loop
       for i from (1+ index) upto (1- item-count) 
       do (setf (elt new-items (1- i))
                (elt items i)))
    (with-slots (items) del (setf items new-items)))
  del)

(defun index-sort-key (n)
  (lambda (x)(elt (items x) n)))

(defmethod sort! ((del delectus) &key (test #'string<)(key #'identity))
  (with-slots (items) del
    (setf items
          (merge-sort (items del) :test test :key key))))

;;; (setq $del (make-delectus '("Foo bar")))
;;; (setq $del1 (make-delectus '("Foo" "bar")))
;;; (setq $del2 (make-delectus '("baz" "grault" "quux" "wibble" "frobbozz")))
;;; (setq $del (make-delectus (list $del1 $del2)))
;;; (count $del1)
;;; (add! $del1 "foobie")
