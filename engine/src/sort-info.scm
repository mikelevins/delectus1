;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort-info.scm
;;;; Project:       Delectus
;;;; Purpose:       how to sort a table
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type sort-info
  id: 2D65FC83-52F9-4A24-BF8E-99A8CA106583
  constructor: %make-sort-info
  column-label
  column-index
  order
  type)

(define (make-sort-info table column-label order type)
  (%make-sort-info column-label (table:column-index table column-label) order type))

