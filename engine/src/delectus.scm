;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       the basic data structure
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; column
;;; ---------------------------------------------------------------------

(define-type column
  constructor: %make-column
  (label read-only:)
  (deleted? unprintable:))

(define (make-column label #!optional (deleted? #f))
  (%make-column label deleted?))

(define (ensure-column thing)
  (cond
   ((column? thing) thing)
   ((string? thing) (make-column thing))
   (else (error "Invalid column label" thing))))

(define (mark-column-deleted! col deleted?)
  (column-deleted?-set! col deleted?))

;;; (define $c (make-column 'foo))
;;; $c

;;; ---------------------------------------------------------------------
;;; row
;;; ---------------------------------------------------------------------

(define-type row
  constructor: %make-row
  (elements unprintable:)
  (deleted? unprintable:))

(define (make-row elts #!optional (deleted? #f))
  (%make-row (vector-with (map identity elts))
             deleted?))

(define (ensure-row thing)
  (cond
   ((row? thing) thing)
   ((list? thing) (make-row thing))
   (else (error "Invalid row data" thing))))

(define (mark-row-deleted! row deleted?)
  (row-deleted?-set! row deleted?))

(define (row-add! r v)
  (row-elements-set! r (vector-append (row-elements r) (vector v)))
  r)

(define (row-element-at row n)
  (vector-ref (row-elements row) n))

(define (row-put-element-at! row n val)
  (vector-set! (row-elements row) n val))

;;; (define $r (ensure-row '("Fred" "Barney")))
;;; (row-elements $r)
;;; (row-element-at $r 1)
;;; (row-add! $r "Wilma")
;;; (row-put-element-at! $r 1 "Betty")

;;; ---------------------------------------------------------------------
;;; delectus
;;; ---------------------------------------------------------------------

(define-type delectus
  constructor: %make-delectus
  (columns columns) 
  (column-indexes column-indexes)
  (rows rows))

(define (parse-columns cols)
  (vector-with (map ensure-column cols)))

(define (parse-rows rows)
  (vector-with (map ensure-row rows)))

(define (make-delectus #!key (columns '())(rows '()))
  (let* ((cols (parse-columns columns))
         (col-count (vector-length cols))
         (indexes (range 0 (length columns)))
         (col-indexes (make-table test: string-ci=?))
         (rows (parse-rows rows)))
    (let loop ((i 0))
      (if (< i col-count)
          (begin
            (table-set! col-indexes (column-label (vector-ref cols i)) i)
            (loop (+ i 1)))))
    (%make-delectus cols col-indexes rows)))

;;; (define $d (make-delectus columns: '("name" "color") rows: '(("fred" "orange")("barney" "brown")("wilma" "white"))))
;;; $d

(define (row-at del row-index)
  (vector-ref (delectus-rows del) row-index))

(define (column-index del label)
  (table-ref (column-indexes del) label #f))

;;; (column-index $d "shape")