;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (ensure-column thing)
  (cond
   ((symbol? thing) thing)
   ((string? thing) (string->symbol thing))
   (else (error "Invalid label" thing))))

;;; ----------------------------------------------------------------------
;;; delectus
;;; ----------------------------------------------------------------------

(define-structure <delectus>
  columns rows)

(define (make-delectus #!key
                       (columns '())
                       (rows '()))
  (let* ((col-count (length columns)))
    (if (every? (lambda (row) (= col-count (length row))) 
                rows)
        (make-<delectus>
         (make-expanding-vector initial-elements: (map ensure-column columns))
         (make-expanding-vector initial-elements: 
                                (map (lambda (row)
                                       (make-expanding-vector initial-elements: row))
                                     rows)))
        (error "incorrect row lengths"))))

;;; (define $d (make-delectus columns: '("name") rows: '(("Fred")("Barney")("Wilma"))))
;;; $d

(define (delectus-columns del)
  (<delectus>-columns del))

(define (delectus-rows del)
  (<delectus>-rows del))

(define (column-index del label)
  (let* ((cols (delectus-columns del))
         (label-count (size cols))
         (label (string->symbol label)))
    (let loop ((i 0))
      (if (< i label-count)
          (if (eq? label (expanding-vector-ref cols i))
              i
              (loop (+ i 1)))
          #f))))

;;; (column-index $d "name")

(define (value-at del label row-index)
  (expanding-vector-ref (expanding-vector-ref (delectus-rows del) row-index)
    (column-index del label)))

(define (put-value-at! del label row-index val)
  (expanding-vector-set! (expanding-vector-ref (delectus-rows del) row-index)
                (column-index del label)
                val))

(define (add-column! del label)
  (if (column-index del label)
      (error "column exists" label)
      (begin
        (add-last! (delectus-columns del)(ensure-column label))
        (expanding-vector-for-each (lambda (r)(add-last! r (expanding-vector-default r)))
                                   (delectus-rows del))
        del)))

(define (add-row! del)
  (add-last! (delectus-rows del)
             (make-expanding-vector capacity: (capacity (delectus-columns del))
                                    initial-elements: (repeat (size (delectus-columns del)) '())))
  del)


;;; (define $d (make-delectus columns: '("name" "color") rows: '(("Fred" "orange")("Barney" "brown")("Betty" "blue"))))
;;; $d
;;; (value-at $d "color" 1)
;;; (put-value-at! $d "color" 1 "pink")
;;; (add-row! $d)
;;; (add-column! $d "gender")
;;; (add-column! $d "home town")
;;; (add-column! $d "bowling league")