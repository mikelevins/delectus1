;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-values.scm
;;;; Project:       Delectus
;;;; Purpose:       values that can be stored in a delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (delectus-value v)
  (cond
   ;; null
   ((null? v) v)
   ;; boolean
   ((boolean? v) v)
   ;; integer
   ((integer? v) v)
   ;; float
   ((flonum? v) v)
   ;; string
   ((string? v) v)
   ;; sequence
   ((seq:sequence? v) v)
   ;; map
   ((map:map? v) v)
   (else (error "Not a valid delectus value" v))))

;;; ----------------------------------------------------------------------
;;; special values
;;; ----------------------------------------------------------------------

(define (nothing) '())
(define (nothing? x) (null? x))
(define (something? x) (not (nothing? x)))

