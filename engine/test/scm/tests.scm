;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Delectus
;;;; Purpose:       test code for engine
;;;; Author:        mikel evins
;;;; Requirements:  delectus loaded
;;;;
;;;; ***********************************************************************

(define (run-all-tests)
  ;; table tests
  (test:table:make)
  (test:table:column-at)
  (test:table:column-index)
  (test:table:count-columns)
  (test:table:row-at)
  (test:table:count-rows)
  (test:table:add-row)
  (test:table:add-column)
  (test:table:put-value-at)
  (test:table:columns-matching)
  (test:table:rows-matching)
  (test:table:row-sort)
  (test:table:mark-deleted)
  (test:table:compact)
  ;; csv tests
  (test:read-csv)
  (newline))

