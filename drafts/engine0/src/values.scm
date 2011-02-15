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
  (or (string->number v)
      v))

;;; ----------------------------------------------------------------------
;;; special values
;;; ----------------------------------------------------------------------

(define (nothing) '())
(define nothing? null?)
(define (something? x) (not (nothing? x)))

