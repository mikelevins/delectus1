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
  #f)

(define (io:from-format-alpha-2 data)
  #f)

(define (io:from-format-alpha-4 data) 
  #f)

(define (io:from-format-beta-2 data)
  #f)

(define (io:from-format-2.0 data)
  data)

(table-set! $data-converters $delectus-format-alpha-1 io:from-format-alpha-1)
(table-set! $data-converters $delectus-format-alpha-2 io:from-format-alpha-2)
(table-set! $data-converters $delectus-format-alpha-4 io:from-format-alpha-4)
(table-set! $data-converters $delectus-format-beta-2 io:from-format-beta-2)
(table-set! $data-converters $delectus-format-2.0 io:from-format-2.0)

