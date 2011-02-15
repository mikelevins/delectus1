;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.scm
;;;; Project:       Delectus
;;;; Purpose:       sequence values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type seq:sequence
  id: BE375B74-4FF8-48D7-B01D-A0EE4228CE7B
  constructor: %make-sequence
  (elements seq:elements))

(define (seq:make elements)
  (%make-sequence (copy-tree elements)))

(define (seq:seq . elements)
  (seq:make elements))

(define (list->seq ls)
  (seq:make ls))

(define (seq->list s)
  (seq:elements s))

(define (vector->seq v)
  (seq:make (vector->list v)))

(define (seq->vector s)
  (list->vector (seq:elements s)))

