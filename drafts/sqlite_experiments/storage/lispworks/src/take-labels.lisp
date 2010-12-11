;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          take-labels.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       generating unbounded numbers of labels
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod string* ((n (eql 0))(s string))
  "")

(defmethod string* ((n integer)(s string))
  (concatenate 'string s (string* s (1- n))))

(defparameter $alphabet
  '("A" "B" "C" "D" "E" "F" "G" "H" "I"
            "J" "K" "L" "M" "N" "O" "P" "Q" "R"
            "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defparameter $alphabet-count 26)

(defun take-label (n)
  (multiple-value-bind (mul index)
      (truncate n $alphabet-count)
    (string* (1+ mul) (elt $alphabet index))))

(defun take-labels (n)
  (loop for i from 0 upto (1- n)
       collect (take-label i)))

;;; (take-label (* 2 1664))
;;; (take-labels 54)

