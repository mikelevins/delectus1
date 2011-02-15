;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          columns.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; column data structure
;;; ----------------------------------------------------------------------

(define-type column
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-column
  (label col:label)
  (index col:index)
  (deleted? col:deleted? col:set-deleted!))

(define (col:make label #!key (index 0)(deleted? #f))
  (%make-column label index deleted?))

(define (col:copy col)
  (%make-column (col:label col) 
                (col:index col)
                (col:deleted? col)))

