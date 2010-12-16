;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          versions.scm
;;;; Project:       Delectus
;;;; Purpose:       delectus version and file-format converions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; constants
;;; ----------------------------------------------------------------------

(define $delectus-format-alpha-1 0)
(define $delectus-format-alpha-2 1)
(define $delectus-format-alpha-4 2)
(define $delectus-format-beta-2 3)
(define $delectus-format-2.0 4)

(define (current-store-format) $delectus-format-2.0)
(define (serialized-store-format s)(vector-ref s 1))

;;; ----------------------------------------------------------------------
;;; Delectus file formats
;;; ----------------------------------------------------------------------

;;; serialized data formats

(define (io:alpha-1-format? data)(store:vector-format? data '<store>))

(define (io:alpha-2-format? data)
  (and (store:vector-format? data 'store)
       (= 4 (vector-length data))))

(define (io:alpha-4-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-alpha-4)))

(define (io:beta-2-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-beta-2)))

(define (io:2.0-format? data)
  (and (delectus? data)
       (= (format data)
          $delectus-format-2.0)))

(define (delectus-data-format data)
  (cond
   ((io:alpha-1-format? data) $delectus-format-alpha-1)
   ((io:alpha-2-format? data) $delectus-format-alpha-2)
   ((io:alpha-4-format? data) $delectus-format-alpha-4)
   ((io:beta-2-format? data) $delectus-format-beta-2)
   ((io:2.0-format? data) $delectus-format-2.0)
   (else #f)))

;;; ----------------------------------------------------------------------
;;; converting from old delectus formats
;;; ----------------------------------------------------------------------

(define $data-converters (make-table))

(define (io:from-format-alpha-1 data)
  (let* ((cols-data (vector-ref data 1))
         (cols (vector-with (map (lambda (cd)(make-column (vector-ref cd 2)
                                                          (vector-ref cd 1)))
                                 cols-data)))
         (col-count (length cols))
         (col-indexes (make-table test: string-ci=?))
         (rows-data (vector-ref data 2))
         (rws (vector-with (map (lambda (rd)(make-row (vector-ref rd 2)
                                                      (vector-ref rd 1)))
                                rows-data))))
    (let loop ((i 0))
      (if (< i col-count)
          (begin
            (table-set! col-indexes (column-label (vector-ref cols i)) i)
            (loop (+ i 1)))))
    (%make-delectus (current-store-format)
                    cols
                    col-indexes
                    rows)))

(define (io:from-format-alpha-2 data)
  (let* ((cols-data (vector-ref data 2))
         (cols (vector-with (map (lambda (cd)(make-column (vector-ref cd 1)
                                                          (vector-ref cd 2)))
                                 cols-data)))
         (col-count (length cols))
         (col-indexes (make-table test: string-ci=?))
         (rows-data (vector-ref data 6))
         (rws (vector-with (map (lambda (rd)(make-row (vector-ref rd 1)
                                                      (vector-ref rd 2)))
                                rows-data))))
    (let loop ((i 0))
      (if (< i col-count)
          (begin
            (table-set! col-indexes (column-label (vector-ref cols i)) i)
            (loop (+ i 1)))))
    (%make-delectus (current-store-format)
                    cols
                    col-indexes
                    rows)))

(define (io:from-format-alpha-4 data) 
  (let* ((cols-data (vector-ref data 2))
         (cols (vector-with (map (lambda (cd)(make-column (vector-ref cd 1)
                                                          (vector-ref cd 3)))
                                 cols-data)))
         (col-count (length cols))
         (col-indexes (make-table test: string-ci=?))
         (rows-data (vector-ref data 6))
         (rws (vector-with (map (lambda (rd)(make-row (vector-ref rd 1)
                                                      (vector-ref rd 2)))
                                rows-data))))
    (let loop ((i 0))
      (if (< i col-count)
          (begin
            (table-set! col-indexes (column-label (vector-ref cols i)) i)
            (loop (+ i 1)))))
    (%make-delectus (current-store-format)
                    cols
                    col-indexes
                    rows)))

(define (io:from-format-beta-2 data)
  (let* ((cols-data (vector-ref data 2))
         (cols (vector-with (map (lambda (cd)(make-column (vector-ref cd 1)
                                                          (vector-ref cd 2)))
                                 cols-data)))
         (col-count (vector-length cols))
         (col-indexes (make-table test: string-ci=?))
         (rows-data (vector-ref data 8))
         (rws (vector-with (map (lambda (rd)(make-row (map (lambda (f)(vector-ref f 1))
                                                           (vector-ref rd 1))
                                                      (vector-ref rd 2)))
                                rows-data))))
    (let loop ((i 0))
      (if (< i col-count)
          (begin
            (table-set! col-indexes (column-label (vector-ref cols i)) i)
            (loop (+ i 1)))))
    (%make-delectus (current-store-format)
                    cols
                    col-indexes
                    rws)))

(define (io:from-format-2.0 data)
  data)

(table-set! $data-converters $delectus-format-alpha-1 io:from-format-alpha-1)
(table-set! $data-converters $delectus-format-alpha-2 io:from-format-alpha-2)
(table-set! $data-converters $delectus-format-alpha-4 io:from-format-alpha-4)
(table-set! $data-converters $delectus-format-beta-2 io:from-format-beta-2)
(table-set! $data-converters $delectus-format-2.0 io:from-format-2.0)

;;; (define $movies #f)
;;; (time (set! $movies (read-delectus-file "/Users/mikel/Projects/delectus/test-data/junior-movies.delectus")))
;;; (time (write-delectus-file $movies "/Users/mikel/Desktop/junior-movies.delectus"))
;;; (define $movies2 #f)
;;; (time (set! $movies2 (read-delectus-file "/Users/mikel/Desktop/junior-movies.delectus")))