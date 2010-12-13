;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Basic data structures
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------
;;; row
;;; ----------------------------------------------------------------------

(define-structure %row %elements %deleted?)

(define (make-row elts)
  (make-%row (vector-with elts) #f))

;;; (make-row '(1))

(define (row . elts)
  (make-row elts))

(define (row? thing)
  (%row? thing))

(define (count row)
  (vector-length (%row-%elements row)))

(define (nth row n)
  (vector-ref (%row-%elements row) n))

(define (set-nth! row n val)
  (vector-set! (%row-%elements row) n val))

(define (row-deleted? row)
  (%row-%deleted? row))

;;; ----------------------------------------------------------------------
;;; column
;;; ----------------------------------------------------------------------

(define-structure %column %label %deleted?)

(define (make-column label)
  (if (string? label)
      (make-%column label #f)
      (error "Invalid column labe" label)))

;;; (make-column "name")

(define (column? thing)
  (%column? thing))

(define (column-deleted? column)
  (%column-%deleted? column))

;;; ----------------------------------------------------------------------
;;; delectus
;;; ----------------------------------------------------------------------

(define-structure %delectus
  %columns %rows)

(define (%ensure-column thing)
  (if (column? thing)
      thing
      (if (string? thing)
          (make-column thing)
          (error "Invalid column specification" thing))))

(define (%parse-columns cols)
  (map %ensure-column cols))

(define (%parse-rows rows)
  (vector-with (map make-row rows)))

(define (make-delectus cols rows)
  (make-%delectus (%parse-columns cols) (%parse-rows rows)))

;;; (define $d (make-delectus '("name" "home") '(("Fred" "Bedrock")("Yogi" "Jellystone")("Bart" "Springfield"))))
;;; (define $dbytes (object->u8vector $d))

