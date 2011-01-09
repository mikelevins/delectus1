;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vectors.scm
;;;; Project:       Delectus
;;;; Purpose:       vector utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (vector-add-last vec val)
  (let* ((oldlen (vector-length vec))
         (newlen (+ 1 oldlen))
         (outvec (make-vector newlen)))
    (let loop ((i 0))
      (if (< i oldlen)
          (begin
            (vector-set! outvec i (vector-ref vec i))
            (loop (+ 1 i)))
          (begin
            (vector-set! outvec oldlen val)
            outvec)))))

(define (vector-map proc vec)
  (list->vector
   (map proc
        (vector->list vec))))

(define (vector-position pred vec val)
  (let ((eltcount (vector-length vec)))
    (let loop ((i 0))
      (if (< i eltcount)
          (if (pred (vector-ref vec i) val)
              i
              (loop (+ 1 i)))
          #f))))

(define (vector-contains? pred vec val)
  (let ((eltcount (vector-length vec)))
    (let loop ((i 0))
      (if (< i eltcount)
          (if (pred (vector-ref vec i) val)
              #t
              (loop (+ 1 i)))
          #f))))
