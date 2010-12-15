;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.scm
;;;; Project:       Delectus
;;;; Purpose:       list utils
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (identity x) x)

(define (copy-list ls)
  (map identity ls))

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

(define (range m n)
  (let loop ((i m)
             (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1)(cons i result)))))

(define (zip ls1 ls2)
  (let loop ((l1 ls1)
             (l2 ls2)
             (result '()))
    (if (or (null? l1)
            (null? l2))
        (reverse result)
        (loop (cdr l1)(cdr l2)
              (cons (cons (car l1)
                          (car l2))
                    result)))))