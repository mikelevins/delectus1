;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       conversion to and from csv format
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $alphabet '("A" "B" "C" "D" "E" "F" "G" "H" "I" 
                          "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                          "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defun extend-alphabet (alpha)
  (apply 'append
         (seq:image (^ (a) 
                      (seq:image (^ (b) (seq:concat a b)) 
                                 alpha))
                    alpha)))

(defun take-letters (n &optional (alphabet $alphabet))
  (if (<= n (length alphabet))
      (seq:take n alphabet)
      (take-letters n (append alphabet (extend-alphabet alphabet)))))

(defmethod read-csv ((path pathname) &key (first-row-headers? t))
  (let ((in-lines nil))
    (csv-parser::do-csv-file ((fields num-fields) path)
      (setf in-lines (cons fields in-lines)))
    (let* ((data-lines (reverse in-lines))
           (cols (if first-row-headers?
                     (car data-lines)
                     (take-letters field-count)))
           (rows (if first-row-headers?
                     (cdr data-lines)
                     data-lines)))
      (make-model :columns cols :rows rows))))

(defmethod read-csv ((path string) &key (first-row-headers? t))
  (read-csv (pathname path) :first-row-headers? first-row-headers?))


