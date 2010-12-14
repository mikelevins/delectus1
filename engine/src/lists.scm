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

(define (every? pred ls)
  (if (null? ls)
      #t
      (if (pred (car ls))
          (if (null? (cdr ls))
              #t
              (every? pred (cdr ls)))
          #f)))

(define (repeat n val)
  (let loop ((i n)
             (result '()))
    (if (<= i 0)
        result
        (loop (- i 1)(cons val result)))))