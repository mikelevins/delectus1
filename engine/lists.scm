;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.scm
;;;; Project:       Delectus
;;;; Purpose:       list utils
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (take n ls)
  (let loop ((i 0)
             (in ls)
             (out '()))
    (if (>= i n)
        (reverse out)
        (if (null? in)
            (error "count out of range" n)
            (loop (+ 1 i)
                  (cdr in)
                  (cons (car in)
                        out))))))

(define (drop n ls)
  (let loop ((i 0)
             (in ls))
    (if (>= i n)
        in
        (if (null? in)
            (error "count out of range" n)
            (loop (+ 1 i)
                  (cdr in))))))