;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort.scm
;;;; Project:       Delectus
;;;; Purpose:       simple merge-sort
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (merge ls1 ls2 test keyfn)
  (cond
   ((null? ls1) ls2)
   ((null? ls2) ls1)
   (else (let ((car1 (car ls1))
               (car2 (car ls2)))
           (if (test (keyfn car1)
                     (keyfn car2))
               (cons car1 (merge (cdr ls1) ls2 test keyfn))
               (cons car2 (merge ls1 (cdr ls2) test keyfn)))))))

(define (merge-sort-list seq test keyfn)
  (if (or (null? seq)
          (null? (cdr seq)))
      seq
      (let ((mid (quotient (length seq) 2)))
        (merge (merge-sort-list (take mid seq) test keyfn)
               (merge-sort-list (drop mid seq) test keyfn)
               test keyfn))))

(define (merge-sort-vector seq test keyfn)
  (list->vector
   (merge-sort-list
    (vector->list seq)
    test keyfn)))

(define (merge-sort seq test keyfn)
  (cond
   ((null? seq) seq)
   ((list? seq) (merge-sort-list seq test keyfn))
   ((vector? seq) (merge-sort-vector seq test keyfn))
   (#t (error "Invalid input to merge-sort" seq))))

