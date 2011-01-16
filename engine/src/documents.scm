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
  (table doc:table)
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
                  (deleted-columns '())
                  (deleted-rows '())
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



(define (doc:add-column! doc label)
  (table:add-column! (doc:table doc) label)
  doc)

(define (doc:add-row! doc)
  (table:add-row! (doc:table doc))
  doc)

(define (doc:value-at doc column-label row-index)
  (table:value-at (doc:table doc)
                  (table:column-index (doc:table doc) column-label) row-index))

(define (doc:put-value-at! doc column-label row-index val)
  (table:put-value-at! (doc:table doc)
                       (table:column-index (doc:table doc) column-label) row-index val))

(define (doc:column-deleted? doc column-label)
  (contains? string-ci=? (document-deleted-columns doc) column-label))

(define (doc:mark-column-deleted! doc column-label deleted?)
  (if deleted?
      (begin
        (if (not (doc:column-deleted? doc column-label))
            (document-deleted-columns-set! doc (cons column-label (document-deleted-columns doc))))
        doc)
      (begin
        (document-deleted-columns-set! doc (remove column-label (document-deleted-columns doc) test: string-ci=?))
        doc)))

(define (doc:row-deleted? doc row-index)
  (contains? = (document-deleted-rows doc) row-index))

(define (doc:mark-row-deleted! doc row-index deleted?)
  (if deleted?
      (begin
        (if (not (doc:row-deleted? doc row-index))
            (document-deleted-rows-set! doc (cons row-index (document-deleted-rows doc))))
        doc)
      (begin
        (document-deleted-rows-set! doc (remove row-index (document-deleted-rows doc) test: =))
        doc)))

(define (doc:show-deleted? doc)
  (document-include-deleted? doc))

(define (doc:set-show-deleted! doc deleted?)
  (document-include-deleted?-set! doc deleted?))

(define (doc:compact-table! doc)
  (table:remove-rows! (document-table doc)
                      (document-deleted-rows doc))
  (table:remove-columns! (document-table doc)
                         (document-deleted-columns doc))
  (document-deleted-rows-set! doc '())
  (document-deleted-columns-set! doc '()))

(define (doc:sort-column doc)
  (document-sort-column doc))

(define (doc:set-sort-column! doc column-label)
  (document-sort-column-set! doc column-label))

(define (doc:sort-order doc)
  (document-sort-order doc))

(define (doc:set-sort-order! doc order)
  (document-sort-order-set! doc order))

(define (doc:sort-type doc)
  (document-sort-type doc))

(define (doc:set-sort-type! doc type)
  (document-sort-type-set! doc type))

(define (doc:filter-text doc)
  (document-filter-text doc))

(define (doc:set-filter-text! doc text)
  (document-filter-text-set! doc text))

