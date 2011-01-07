;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-views.scm
;;;; Project:       Delectus
;;;; Purpose:       data structures for selecting subsets of delectus tables
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; A delectus-view is a computed subset of a delectus-table.
;;; views are used to store, for examples, the results of filtering or
;;; sorting the contents of tables without altering the tables
;;; themselves.

(define-type delectus-view
  id: AFEF4755-2E5D-4849-A0C3-59726DAA59A5
  constructor: %make-delectus-view
  table
  columns
  rows)

;;; ----------------------------------------------------------------------
;;; view API
;;; ----------------------------------------------------------------------

;;; constructors

(define (view:select dtable column-indexes row-indexes)
  (%make-delectus-view dtable
                       (list->vector column-indexes)
                       (list->vector row-indexes)))

;;; orderer: (lambda (columns row1 row2)...) => #t | #f

(define (view:sort dview orderer)
  (%make-delectus-view (delectus-view-table dview)
                       (delectus-view-columns dview)
                       (vector:sort (delectus-view-rows dview)
                                    (partial orderer (delectus-view-columns dview)))))

;;; accessors

(define (view:row-at dview row-index)
  (vector-ref (table:rows (delectus-view-table dview))
              (vector-ref (delectus-view-rows dview) row-index)))

(define (view:column-at dview column-index)
  (vector-ref (delectus-view-columns dview)
              column-index))

(define (view:column-index dview label)
  (vector-position string-ci=?
                   (delectus-view-columns dview)
                   label))

(define (view:count-columns dview)
  (vector-length (delectus-view-columns dview)))

(define (view:count-rows dview)
  (vector-length (delectus-view-rows dview)))

(define (view:value-at dview column-index row-index)
  (vector-ref (vector-ref (table:rows (delectus-view-table dview))
                          (vector-ref (delectus-view-rows dview) row-index))
              column-index))

(define (view:put-value-at! dview column-index row-index val)
  (vector-set! (vector-ref (table:rows (delectus-view-table dview))
                           (vector-ref (delectus-view-rows dview) row-index))
               column-index
               val))

