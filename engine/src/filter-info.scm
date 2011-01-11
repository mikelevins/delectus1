;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter-info.scm
;;;; Project:       Delectus
;;;; Purpose:       info used to filter table data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type filter-info
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-filter-info
  text)

(define (make-filter-info tx)
  (%make-filter-info tx))