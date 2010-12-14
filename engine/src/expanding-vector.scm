;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          expanding-vector.scm
;;;; Project:       Delectus
;;;; Purpose:       an implementation of growable vectors
;;;;                optimized for the Delectus use case (i.e.
;;;;                infrequent growth by small increments)
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $default-expanding-vector-capacity 4)

(define-structure <expanding-vector>
  elts fill-pointer default)

(define (make-expanding-vector #!key 
                               (capacity $default-expanding-vector-capacity)
                               (initial-elements '())
                               (default-element '()))
  (let* ((elt-count (length initial-elements))
         (capacity (if (<= elt-count $default-expanding-vector-capacity)
                       $default-expanding-vector-capacity
                       (* $default-expanding-vector-capacity
                          (+ 1 (quotient elt-count $default-expanding-vector-capacity)))))
         (elt-vec (make-vector capacity default-element)))
    (let loop ((i 0)
               (elts initial-elements))
      (if (not (null? elts))
          (begin
            (vector-set! elt-vec i (car elts))
            (loop (+ i 1)(cdr elts)))))
    (make-<expanding-vector> elt-vec elt-count default-element)))

;;; (define $ev (make-expanding-vector initial-elements: '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
;;; $ev
;;; (capacity $ev)

(define (expanding-vector-default ev)
  (<expanding-vector>-default ev))

(define (size ev)
  (<expanding-vector>-fill-pointer ev))

(define (capacity ev)
  (vector-length (<expanding-vector>-elts ev)))

;;; (size $ev)
;;; (capacity $ev)

(define (expand! ev)
  (let ((new-vec (make-vector (+ (capacity ev)
                                 $default-expanding-vector-capacity) 
                              (<expanding-vector>-default ev)))
        (elt-count (size ev))
        (elts (<expanding-vector>-elts ev)))
    (let loop ((i 0))
      (if (>= i elt-count)
          i
          (begin
            (vector-set! new-vec i (vector-ref elts i))
            (loop (+ i 1)))))
    (<expanding-vector>-elts-set! ev new-vec))
  ev)

(define (add-last! ev val)
  (if (>= (size ev)(capacity ev))
      (begin
        (expand! ev)
        (add-last! ev val))
      (begin
        (vector-set! (<expanding-vector>-elts ev) (size ev) val)
        (<expanding-vector>-fill-pointer-set! ev (+ 1 (<expanding-vector>-fill-pointer ev)))
        ev)))

(define (expanding-vector-ref ev n)
  (if (< n (size ev))
      (vector-ref (<expanding-vector>-elts ev) n)
      (error "index out of range" n)))

(define (expanding-vector-set! ev n v)
  (if (< n (size ev))
      (begin
        (vector-set! (<expanding-vector>-elts ev) n v)
        ev)
      (error "index out of range" n)))

(define (expanding-vector-for-each fn ev)
  (let ((elt-count (size ev)))
    (let loop ((i 0))
      (if (< i elt-count)
          (begin
            (fn (expanding-vector-ref ev i))
            (loop (+ i 1)))
          ev))))


;;; $ev
;;; (expanding-vector-set! $ev 25 'z)