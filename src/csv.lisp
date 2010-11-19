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
  (let ((m (make-model :columns nil :rows nil)))
    (csv-parser::do-csv-file ((fields num-fields) path)
      (setf (rows m)(seq:add-last (rows m) (ensure-row fields))))
    (let ((field-count (seq:length (elements (seq:element (rows m) 0)))))
      (assert (seq:every? (lambda (row)(= field-count (seq:length (elements row)))) 
                          (rows m))()
                          "Malformed csv data read from file '~A'" path)
      (if first-row-headers?
          (progn
            (setf (columns m)(seq:element (rows m) 0))
            (setf (rows m)(seq:drop 1 (rows m)))
            m)
          (progn
            (setf (columns m)(ensure-columns (take-letters field-count)))
            m)))))

(defmethod read-csv ((path string) &key (first-row-headers? t))
  (read-csv (pathname path) :first-row-headers? first-row-headers?))


