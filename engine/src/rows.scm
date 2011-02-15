;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rows.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; row data structure
;;; ----------------------------------------------------------------------

(define-type row
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-row
  (elements row:elements row:set-elements!)
  (index row:index row:set-index!)
  (deleted? row:deleted? row:set-deleted!))

(define (row:make #!key (elements '())(index 0)(deleted? #f))
  (%make-row (list->vector elements) index deleted?))

(define (row:add-last! row val)
  (row:set-elements! row (vector-add-last (row:elements row) val)))

(define (row:element row index)
  (vector-ref (row:elements row) index))

(define (row:select-elements row row-indexes)
  (let ((new-elts (vector-select (row:elements row) row-indexes)))
    (%make-row new-elts (row:index row) (row:deleted? row))))
