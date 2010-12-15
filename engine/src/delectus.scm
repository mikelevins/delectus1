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
   ((symbol? thing) (make-column thing))
   ((string? thing) (ensure-column (string->symbol thing)))
   (else (error "Invalid column label" thing))))

(define (mark-column-deleted! col deleted?)
  (column-deleted?-set! col deleted?))

;;; (define $c (make-column 'foo))

;;; ---------------------------------------------------------------------
;;; row
;;; ---------------------------------------------------------------------

(define-type row
  constructor: %make-row
  (elements unprintable:)
  (deleted? unprintable:))

(define (make-row elts #!optional (deleted? #f))
  (%make-row (make-sequence initial-contents: elts)
             deleted?))

(define (ensure-row thing)
  (cond
   ((row? thing) thing)
   ((list? thing) (make-row thing))
   (else (error "Invalid row data" thing))))

(define (mark-row-deleted! row deleted?)
  (row-deleted?-set! row deleted?))

(define (row-add! r v)
  (sequence-add! (row-elements r) v)
  r)

(define (row-element-at row n)
  (if (>= n (wt-tree/size (row-elements row)))
      (error "Index out of range" n)
      (wt-tree/index-datum (row-elements row) n)))

(define (row-put-element-at! row n val)
  (if (>= n (wt-tree/size (row-elements row)))
      (error "Index out of range" n)
      (let ((key (wt-tree/index (row-elements row) n)))
        (wt-tree/add! (row-elements row) key val))))

(define (row->list row)
  (let ((result '()))
    (wt-tree/for-each (lambda (k v)(set! result (cons v result))) 
                      (row-elements row))
    (reverse result)))

;;; (define $r (ensure-row '("Fred" "Barney")))
;;; (row->list $r)
;;; (row-element-at $r 1)
;;; (row-add! $r "Wilma")

;;; ---------------------------------------------------------------------
;;; delectus
;;; ---------------------------------------------------------------------

(define-type delectus
  constructor: %make-delectus
  (columns unprintable:)
  (rows unprintable:))

(define (parse-columns cols)
  (map ensure-column cols))

(define (parse-rows rows)
  (map ensure-row rows))

(define (make-delectus #!key (columns '())(rows '()))
  (let* ((cols (parse-columns columns))
         (col-tree (make-sequence initial-contents: cols))
         (rows (parse-rows rows))
         (row-tree (make-sequence initial-contents: rows)))
    (%make-delectus col-tree row-tree)))

;;; (define $d (make-delectus columns: '("name" "color") rows: '(("fred" "orange")("barney" "brown")("wilma" "white"))))
;;; (print-sequence (delectus-columns $d))
;;; (sequence-for-each (lambda (r)(print-sequence (row-elements r))) (delectus-rows $d))