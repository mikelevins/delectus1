;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-tables.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; table data structure
;;; ----------------------------------------------------------------------

(define-type delectus-table
  id: 045F1C7B-788D-4F47-A325-BDB0D60EF205
  constructor: %make-delectus-table
  (rows table:rows table:set-rows!)
  (columns table:columns table:set-columns!))

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

(define (table:make #!key (columns '())(rows '()))
  (let* ((rows (%parse-rows rows))
         (cols (%parse-columns columns)))
    (%make-delectus-table rows cols)))

;;; ----------------------------------------------------------------------
;;; table API
;;; ----------------------------------------------------------------------

(define (table:row-at del row-index)
  (vector-ref (table:rows del)
              row-index))

(define (table:column-at del column-index)
  (vector-ref (table:columns del) column-index))

(define (table:column-index del label)
  (vector-position string-ci=? (table:columns del) label))

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
        (table:set-columns! del (vector-add-last (table:columns del) label))
        (table:set-rows! del (vector-map (lambda (row)(vector-add-last row (nothing)))
                                         (table:rows del)))
        del)))

(define (table:value-at del column-index row-index)
  (vector-ref (vector-ref (table:rows del) row-index) column-index))

(define (table:put-value-at! del column-index row-index val)
  (let ((val (delectus-value val)))
    (vector-set! (vector-ref (table:rows del) row-index) column-index val)
    del))

