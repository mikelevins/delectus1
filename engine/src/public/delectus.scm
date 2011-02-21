;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       data engine
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (delectus:new)
  (reg:register-delectus! (delectus:make)))

(define (delectus:get-view id #!key (description #f))
  (reg:id->view id description: description))

(define (delectus:value-at id column-label row-index)
  (table:value-at (reg:id->view id) column-label row-index))

(define (delectus:put-value-at! id column-label row-index val)
  (table:put-value-at! (reg:id->view id) column-label row-index))

(define (delectus:add-row! id)
  (table:add-row! (reg:id->base-table id))
  (reg:invalidate-view! id))

(define (delectus:add-column! id column-label)
  (table:add-column! (reg:id->base-table id) column-label)
  (reg:invalidate-view! id))

(define (delectus:mark-column-deleted! id column-label deleted?)
  (table:mark-column-deleted! (reg:id->view id) column-label deleted?)
  (reg:invalidate-view! id))

(define (delectus:mark-row-deleted! id row-index deleted?)
  (table:mark-row-deleted! (reg:id->view id) row-index deleted?)
  (reg:invalidate-view! id))

(define (delectus:compact! id)
  (table:compact! (reg:id->base-table id))
  (reg:invalidate-view! id))

(define (delectus:write id path)
  (io:write-data (io:delectus->bytes (reg:id->base-table id))
                  path))

(define (delectus:read path)
  (reg:register-delectus! (io:bytes->delectus (io:read-delectus path))))

(define (delectus:write-csv id path)
  (io:write-data (io:delectus->csv (reg:id->base-table id))
                  path))

(define (delectus:read-csv path)
  (reg:register-delectus! (io:csv->delectus (io:read-csv path))))

