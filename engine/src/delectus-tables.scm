;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-tables.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (nothing) '())
(define (nothing? x) (null? x))
(define (something? x) (not (nothing? x)))

(define-type delectus-table
  id: 045F1C7B-788D-4F47-A325-BDB0D60EF205
  constructor: %make-delectus-table
  (rows table:rows table:set-rows!)
  (columns table:columns table:set-columns!)
  (column-label->index-table table:column-label->index-table))

(define (%parse-rows rows)
  (list->vector
   (map (lambda (row)(map delectus-value row))
        rows)))

(define (delectus-column-label thing)
  (if (string? thing)
      thing
      (error "Invalid column label" thing)))

(define (%parse-columns columns)
  (let ((cols (map delectus-column-label columns)))
    (if (duplicates? string-ci=? cols)
        (error "Duplicate labels" cols)
        (list->vector cols))))

(define (%make-label->index-table cols)
  (let ((col-count (vector-length cols))
        (tbl (make-table)))
    (let ((i 0))
      (if (< i col-count)
          (table-set! tbl (vector-ref cols i) i)
          tbl))))

(define (table:make #!key (columns '())(rows '()))
  (let* ((rows (%parse-rows rows))
         (cols (%parse-columns columns))
         (col-index-table (%make-label->index-table cols)))
    (%make-delectus-table rows cols col-index-table)))

(define (table:column-label->index del label)
  (table-ref (table:column-label->index-table del) label #f))

(define (table:count-columns del)
  (vector-length (table:columns del)))

(define (table:count-rows del)
  (vector-length (table:rows del)))

(define (table:add-row! del)
  (table:set-rows! del
                   (vector-add-last (table:rows del)
                                    (make-vector (table:count-columns del) (nothing))))
  del)

(define (table:add-column! del label)
  (if (vector-contains? string-ci=? (table:columns del) label)
      (error "Column exists" label)
      (begin
        (table-set! (table:column-label->index-table del)
                    label (table:count-columns del))
        (table:set-columns! del
                            (vector-add-last (table:columns del) label))
        (table:set-rows! del
                         (vector-map (lambda (row)(vector-add-last row (nothing)))
                                     (table:rows del)))
        del)))

(define (table:row-at del row-index)
  (vector-ref (table:rows del)
              row-index))

(define (table:value-at del column-index row-index)
  (vector-ref (vector-ref (table:rows del) row-index) column-index))

(define (table:put-value-at! del column-index row-index val)
  (vector-set! (vector-ref (table:rows del) row-index) column-index val)
  del)

(define (table:column-at del column-index)
  (let ((row-count (table:count-rows del)))
    (let loop ((i 0)
               (result '()))
      (if (< i row-count)
          (loop (+ i 1)
                (cons (table:value-at del column-index i)
                      result))
          (reverse result)))))

;;; (define $t (table:make))
;;; (table:count-columns $t)
;;; (table:count-rows $t)
;;; (table:add-row! $t)
;;; (table:add-column! $t "Name")
;;; (table:add-column! $t "Shape")
;;; $t
;;; (table:row-at $t 1)
;;; (table:column-at $t (table:column-label->index $t "Name"))
;;; (table:put-value-at! $t (table:column-label->index $t "Name") 0 "Fred")
;;; (table:put-value-at! $t (table:column-label->index $t "Name") 1 "Wilma")
