;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       sorting utilities
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; worst-case times for the built-in sort in Lispworks are hideous for large data

(defmethod empty? ((x vector))(zerop (length x)))
(defmethod empty? ((x list))(null x))

(defmethod single? ((x vector))(= 1 (length x)))
(defmethod single? ((x list))(null (cdr x)))

(defmethod merge-sort ((sequence vector) &key (test #'<)(key #'identity))
  (if (or (empty? sequence) (single? sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge 'vector
               (merge-sort (subseq sequence 0 half) :test test :key key)
               (merge-sort (subseq sequence half) :test test :key key)
               test 
               :key key))))

(defmethod merge-sort ((sequence list) &key (test #'<)(key #'identity))
  (if (or (null sequence) (single? sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge 'list
               (merge-sort (subseq sequence 0 half) :test test :key key)
               (merge-sort (subseq sequence half) :test test :key key)
               test 
               :key key))))
