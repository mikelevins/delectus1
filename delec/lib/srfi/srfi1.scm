;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          srfi1.scm
;;;; Project:       Delectus
;;;; Purpose:       selected functions from SRFI1: lists
;;;; Author:        various (as noted)
;;;;                selected by mikel evins
;;;; ***********************************************************************


;;; FROM:
;;; SRFI-1 list-processing library 			-*- Scheme -*-
;;; Reference implementation
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin
;;; check-arg removed --mikel

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].

(define (car+cdr pair) (values (car pair) (cdr pair)))

(define (%cars+cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values '() '()))))))


;;; This is a legal definition which is fast and sloppy:
;;;     (define null-list? not-pair?)
;;; but we'll provide a more careful one:
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))
           
(define (take k lis)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (drop k lis)(list-tail lis k))

(define (filter pred lis)			; Sleazing with EQ? makes this
  (let recur ((lis lis))		
    (if (null-list? lis) lis			; Use NOT-PAIR? to handle dotted lists.
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail)))	; Replicate the RECUR call so
                (if (eq? tail new-tail) lis
                    (cons head new-tail)))
              (recur tail))))))			; this one can be a tail call.

(define (any pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
        (and (pair? heads)
             (let lp ((heads heads) (tails tails))
               (receive (next-heads next-tails) (%cars+cdrs tails)
                 (if (pair? next-heads)
                     (or (apply pred heads) (lp next-heads next-tails))
                     (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (and (not (null-list? lis1))
           (let lp ((head (car lis1)) (tail (cdr lis1)))
             (if (null-list? tail)
                 (pred head)		; Last PRED app is tail call.
                 (or (pred head) (lp (car tail) (cdr tail))))))))

(define (every pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
        (or (not (pair? heads))
            (let lp ((heads heads) (tails tails))
              (receive (next-heads next-tails) (%cars+cdrs tails)
                (if (pair? next-heads)
                    (and (apply pred heads) (lp next-heads next-tails))
                    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-list? lis1)
          (let lp ((head (car lis1))  (tail (cdr lis1)))
            (if (null-list? tail)
                (pred head)	; Last PRED app is tail call.
                (and (pred head) (lp (car tail) (cdr tail))))))))