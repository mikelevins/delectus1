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

(defun make-row-sort-key (n)(lambda (el)(elt el n)))
(defun make-index-sort-key (n)(lambda (el)(elt (elt rows ) n)))

(defun sort-rows (rows index comparator)
  (merge-sort rows :key (make-row-sort-key index) :test comparator))

(defmethod index-comparator ((sort-type (eql :alphabetical))(reversed? (eql nil)))
  #'string<)

(defmethod index-comparator ((sort-type (eql :alphabetical))(reversed? (eql t)))
  #'string>)

(defmethod index-comparator ((sort-type (eql :numeric))(reversed? (eql nil)))
  #'<)

(defmethod index-comparator ((sort-type (eql :numeric))(reversed? (eql t)))
  #'>)

(defun sort-indexes (pres indexes)
  (merge-sort indexes :key (make-index-sort-key)
              :test (index-comparator (sort-type pres)(sort-reversed? pres))))

;;; (merge-sort '("Foo" "Bar" "Baz" "Quux" "Grault") :test #'string<)