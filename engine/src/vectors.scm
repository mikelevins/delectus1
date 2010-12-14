;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vectors.scm
;;;; Project:       Delectus
;;;; Purpose:       vector utils
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (vector-with elts)
  (let* ((elt-count (length elts))
         (vec (make-vector elt-count)))
    (let loop ((i 0)
               (elts elts))
      (if (null? elts)
          vec
          (begin
            (vector-set! vec i (car elts))
            (loop (+ i 1)(cdr elts)))))))

(define (vector-for-each fn vec)
  (let ((elt-count (vector-length vec)))
    (let loop ((i 0))
      (if (< i elt-count)
          (fn (vector-ref vec i))
          vec))))

