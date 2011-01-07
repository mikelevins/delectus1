;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort.scm
;;;; Project:       Delectus
;;;; Purpose:       sorting utils
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $sort-type-none 0)
(define $sort-type-numeric 1)
(define $sort-type-alphabetical 2)

(define $sort-order-ascending 0)
(define $sort-order-descending 1)

(define-type sort-info
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-sort-info
  sort-column
  sort-order
  sort-type)

(define (make-sort-info column-index order type)
  (%make-delectus-sort-info column-index order type))

(define (get-comparator sort-order sort-type)
  (cond
   ((= sort-order $sort-order-ascending)
    (cond
     ((= sort-type $sort-type-none) (always #t))
     ((= sort-type $sort-type-numeric) <)
     ((= sort-type $sort-type-alphabetical) string-ci<?)
     (else (error "Unknown sort type" sort-type))))
   ((= sort-order $sort-order-descending)
    (cond
     ((= sort-type $sort-type-none) (always #t))
     ((= sort-type $sort-type-numeric) >)
     ((= sort-type $sort-type-alphabetical) string-ci>?)
     (else (error "Unknown sort type" sort-type))))
   (else (error "Unknown sort order" sort-order))))

(define (make-comparator input-rows sort-info)
  (let ((key (lambda (index)
               (vector-ref (vector-ref rows index)
                           (sort-info-sort-column sort-info))))
        (compare (get-comparator (sort-info-sort-order sort-info)
                                 (sort-info-sort-type sort-info))))
    (lambda (n1 n2)
      (compare (key n1)(key n2)))))

;;; given a vector of rows and a sort-info, return a vector of row
;;; indexes representing the rows in the order determined by the
;;; sort-info.

(define (sort:ordered-rows input-rows sort-info)
  (let ((indexes (list->vector (range 0 (vector-length input-rows)))))
    (sort indexes (make-comparator input-rows sort-info))))

