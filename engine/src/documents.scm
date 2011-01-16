;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          documents.scm
;;;; Project:       Delectus
;;;; Purpose:       user-oriented documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; document data structure
;;; ----------------------------------------------------------------------

(define-type document
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-document
  format-version
  table
  deleted-columns
  deleted-rows
  include-deleted?
  sort-column
  sort-order
  sort-type
  filter-text
  view
  view-valid?)

(define (doc:make #!key 
                  (table #f)
                  (deleted-columns #f)
                  (deleted-rows #f)
                  (include-deleted? #f)
                  (sort-column #f)
                  (sort-order #f)
                  (sort-type #f)
                  (filter-text #f))
  (%make-document
   $delectus-format-1.0
   table
   deleted-columns
   deleted-rows
   include-deleted?
   sort-column
   sort-order
   sort-type
   filter-text
   #f
   #f))