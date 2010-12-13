;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort.scm
;;;; Project:       Delectus
;;;; Purpose:       simple merge-sort
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (merge-sort-list seq test keyfn)
  (if (null? seq)
      seq
      (receive (left right)(split-list seq)
               (merge-lists (merge-sort-list left test keyfn)
                            (merge-sort-list left test keyfn)
                            test keyfn))))

(define (merge-sort-vector seq test keyfn)
  (list->vector
   (merge-sort-list
    (vector-list seq)
    test keyfn)))

(define (merge-sort seq test keyfn)
  (cond
   ((null? seq) seq)
   ((list? seq) (merge-sort-list seq test keyfn))
   ((vector? seq) (merge-sort-vector seq test keyfn))
   (#t (error "Invalid input to merge-sort" seq))))

