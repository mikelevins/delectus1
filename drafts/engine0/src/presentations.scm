;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          presentations.scm
;;;; Project:       Delectus
;;;; Purpose:       Filtered and sorted presentations of table data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; Private
;;; ----------------------------------------------------------------------

;;; presentation data structure

(define-type delectus-presentation
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-delectus-presentation
  (table pres:table)
  (filter-text pres:%filter-text pres:%set-filter-text!)
  (sort-column pres:%sort-column pres:%set-sort-column!)
  (sort-order pres:%sort-order pres:%set-sort-order!)
  (sort-type pres:%sort-type pres:%set-sort-type!)
  (cache pres:%cache pres:%set-cache!))

(define (pres:make #!key
                   (table (table:make))
                   (filter-text #f)
                   (sort-column #f)
                   (sort-order #f)
                   (sort-type #f)
                   (cache #f))
  (%make-delectus-presentation table filter-text sort-column sort-order sort-type cache))

;;; ----------------------------------------------------------------------
;;; Public access API
;;; ----------------------------------------------------------------------

(define (pres:value-at pres column-label row-index)
  (%update-cache! pres)
  (let ((tbl (pres:%cache pres)))
    (table:value-at tbl column-label row-index)))

(define (pres:put-value-at! pres column-label row-index val)
  )

;;; ----------------------------------------------------------------------
;;; Public filtering API
;;; ----------------------------------------------------------------------

(define (pres:filter-text pres)
  )

(define (pres:set-filter-text! pres text)
  )

;;; ----------------------------------------------------------------------
;;; Public restructuring API
;;; ----------------------------------------------------------------------

(define (pres:add-row! pres)
  )

(define (pres:add-column! pres label)
  )

(define (pres:mark-column-deleted! pres label deleted?)
  )

(define (pres:mark-row-deleted! pres row-index deleted?)
  )

(define (pres:compact! pres)
  )


;;; ----------------------------------------------------------------------
;;; Public sorting API
;;; ----------------------------------------------------------------------

(define (pres:sort-column pres)
  )

(define (pres:set-sort-column! pres column-label)
  )

(define (pres:sort-order pres)
  )

(define (pres:set-sort-order! pres column-label)
  )

