;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequence.scm
;;;; Project:       Delectus
;;;; Purpose:       a sequence type built in weight-balanced trees
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; sequence
;;; ---------------------------------------------------------------------

(define %sequence-tree (make-wt-tree-type <))

(define-type <sequence>
  constructor: %make-sequence
  (elements elements unprintable:))

(define (make-sequence #!key
                       (initial-size #f)
                       (initial-contents #f)
                       (initial-element #f))
  (let ((initial-size (if initial-contents
                          (begin
                            (if initial-size (error "use initial-size: or initial-contents: but not both"))
                            (if initial-element (error "use initial-element: or initial-contents: but not both"))
                            (length initial-contents))
                          (or initial-size 0)))
        (tree (make-wt-tree %sequence-tree)))
    (if initial-contents
        (let loop ((elts initial-contents)
                   (i 0))
          (if (null? elts)
              '()
              (begin
                (wt-tree/add! tree i (car elts))
                (loop (cdr elts)(+ i 1)))))
        (let loop ((i 0))
          (if (< i initial-size)
              (begin
                (wt-tree/add! tree i initial-element)
                (loop (+ i 1))))))
    (%make-sequence tree)))

;;; (define $s (make-sequence))
;;; (define $s (make-sequence initial-size: 5))
;;; (define $s (make-sequence initial-contents: '(a b c d)))
;;; (print-sequence $s)

(define (sequence . elts)
  (make-sequence initial-contents: elts))

;;; (print-sequence (sequence))
;;; (print-sequence (sequence 0 1 2 3))

(define (print-sequence seq #!optional (out (current-output-port)))
  (display "#<sequence ( ")
  (wt-tree/for-each (lambda (k v)(display v)(display " "))
                    (elements seq))
  (display ")>"))

(define (sequence-length seq)
  (wt-tree/size (elements seq)))

(define (sequence-max-key seq)
  (wt-tree/index (elements seq)
                 (- (wt-tree/size (elements seq)) 1)))

(define (sequence-check-range seq i)
  (if (not (<= (wt-tree/min (elements seq))
               i
               (sequence-max-key seq)))
      (error "index out of range" i)))

(define (sequence-ref seq i)
  (sequence-check-range seq i)
  (wt-tree/index-datum (elements seq) i))

(define (sequence-set! seq i v)
  (sequence-check-range seq i)
  (wt-tree/add! (elements seq)
                (wt-tree/index (elements seq) i)
                v)
  seq)

(define (sequence-add! seq v)
  (wt-tree/add! seq (+ 1 (sequence-max-key seq)) v))

(define (sequence-delete! seq n)
  (wt-tree/delete! (elements seq) (wt-tree/index (elements seq) n)))

(define (sequence-for-each fn seq)
  (wt-tree/for-each (lambda (k v)(fn v))
                    (elements seq)))
