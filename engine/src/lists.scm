;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.scm
;;;; Project:       Delectus
;;;; Purpose:       list utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (repeat n val)
  (let loop ((counter n)
             (result '()))
    (if (<= counter 0)
        (reverse result)
        (loop (- counter 1)
              (cons val result)))))

(define (copy-tree x)
  (if (list? x)
      (map copy-tree x)
      x))