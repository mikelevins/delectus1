;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io-formats.scm
;;;; Project:       Delectus
;;;; Purpose:       conversion from older file formats
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (store:vector-format? x tag)
  (and (vector? x)
       (> (vector-length x) 1)
       (eqv? tag (vector-ref x 0))))

(define (serialized-store? s)(store:vector-format? s 'store))
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

(define (delectus-format data)
  (cond
   ((document? data) $delectus-format-1.0)
   ((io:alpha-1-format? data) $delectus-format-alpha-1)
   ((io:alpha-2-format? data) $delectus-format-alpha-2)
   ((io:alpha-4-format? data) $delectus-format-alpha-4)
   ((io:beta-2-format? data) $delectus-format-beta-2)
   (else #f)))

;;; ----------------------------------------------------------------------
;;; converting delectus formats
;;; ----------------------------------------------------------------------

(define $data-converters (make-table))

(define (io:from-format-alpha-1 data)
  (store:make
   version: (current-store-format)     ; version
   columns: (map (lambda (c)           ; columns
                   (make-column
                    (vector-ref c 2) ; label
                    (vector-ref c 1))) 
                 (vector-ref data 1))
   show-deleted: #f
   sort-column: #f ; sort-column
   sort-reversed: #f ; sort-reversed?
   rows: (map (lambda (r)           ; rows
                (make-row
                 (map make-field (vector-ref r 2)) ; fields
                 (vector-ref r 1))) 
              (vector-ref data 2))
   notes: #f))

(define (io:from-format-alpha-2 data)
  (store:make
   version: (current-store-format)     ; version
   columns: (map (lambda (c)           ; columns
                   (make-column
                    (vector-ref c 1) ; label
                    (vector-ref c 2))) 
                 (vector-ref data 2))
   show-deleted: #f ; show-deleted?
   column-layout: '() ; column-layout
   to-do-column: #f ; to-do-column
   sort-column: #f
   sort-reversed: #f
   rows: (map (lambda (r)           ; rows
                (make-row
                 (map (lambda (f) (make-field (vector-ref f 1))) ; fields
                      (vector-ref r 1))
                 (vector-ref r 2))) 
              (vector-ref data 6))
   notes: (vector-ref data 7)))

(define (io:from-format-alpha-4 data) 
  (store:make
   version: (current-store-format)
   columns: (map (lambda (c)
                   (make-column
                    (vector-ref c 1) ; label
                    (vector-ref c 3))) 
                 (vector-ref data 2))
   show-deleted: #f
   column-layout: '()
   to-do-column: #f ; to-do-column
   sort-column: #f
   sort-reversed: #f
   rows: (map (lambda (r)
                (make-row
                 (map (lambda (f) (make-field (vector-ref f 1))) ; fields
                      (vector-ref r 1))
                 (vector-ref r 2))) 
              (vector-ref data 6))
   notes: (vector-ref data 7)))

(define (io:from-format-beta-2 data) 
  (let* ((format-version (vector-ref data 1))
         (columns-data (vector-ref data 2))
         (columns (map (lambda (cdata)(%make-column (vector-ref cdata 1)(vector-ref cdata 2))) 
                       columns-data))
         (colseq (%make-column-sequence (list->vector columns)))
         (include-deleted? (vector-ref data 3))
         (column-layout (vector-ref data 4))
         (window-layout (vector-ref data 5))
         (sort-column (vector-ref data 6))
         (sort-reversed? (vector-ref data 7))
         (rows-data (vector-ref data 8))
         (rids (range 0 (length rows-data)))
         (rows (map (lambda (rdata rid) 
                      (let* ((fields-data (vector-ref rdata 1))
                             (entries (map (lambda (fd)(entry:make (vector-ref fd 1)))
                                           fields-data))
                             (rdel? (vector-ref rdata 2)))
                        (%make-row (list->vector entries) rid #f #f))) 
                    rows-data rids))
         (notes (vector-ref data 9))
         (tbl (%make-delectus-table colseq (list->vector rows))))
    (%make-document tbl #f #f #f include-deleted?
                    #f sort-column (if sort-reversed? $SORT_DESCENDING $SORT_ASCENDING))))

(table-set! $data-converters $delectus-format-alpha-1 io:from-format-alpha-1)
(table-set! $data-converters $delectus-format-alpha-2 io:from-format-alpha-2)
(table-set! $data-converters $delectus-format-alpha-4 io:from-format-alpha-4)
(table-set! $data-converters $delectus-format-beta-2 io:from-format-beta-2)

(define (converter-for-format data)
  (table-ref $data-converters (delectus-format data) #f))

(define (data->document data)
  (let ((converter (converter-for-format data)))
    (converter data)))

