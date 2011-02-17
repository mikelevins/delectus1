;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       data engine
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; a view parameter is a delectus-table. it may be the underlying
;;; data table, or a computed view of that table.

(define (delectus:value-at view column-label row-index)
  (row:element (table:row-at view row-index)
               (column-sequence:position (table:column-sequence view) column-label)))

(define (delectus:put-value-at! view column-label row-index val)
  (row:set-element! (table:row-at view row-index)
                    (column-sequence:position (table:column-sequence view) column-label)
                    val))

(define (delectus:add-row! view)
  (let ((tbl (reg:get-table view)))
    (table:add-row! tbl)
    (reg:invalidate-view! view)
    tbl))

(define (delectus:add-column! view column-label)
  (let ((tbl (reg:get-table view)))
    (table:add-column! tbl column-label)
    (reg:invalidate-view! view)
    tbl))

(define (delectus:mark-column-deleted! view column-label deleted?)
  (let ((tbl (reg:get-table view)))
    (table:mark-column-deleted! tbl column-label deleted?)
    (reg:invalidate-view! view)
    tbl))

(define (delectus:mark-row-deleted! view row-index deleted?)
  (let ((tbl (reg:get-table view)))
    (table:mark-row-deleted! tbl row-index deleted?)
    (reg:invalidate-view! view)
    tbl))

(define (delectus:compact! view)
  (table:compact! (reg:get-table view)))

(define (delectus:write view path)
  )

(define (delectus:read path)
  )

(define (delectus:write-csv view path)
  )

(define (delectus:read-csv path)
  )

