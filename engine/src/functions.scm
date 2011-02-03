;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Delectus
;;;; Purpose:       function utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))