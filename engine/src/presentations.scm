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
  (filter-text pres:filter-text pres:set-filter-text!)
  (sort-column pres:sort-column pres:set-sort-column!)
  (sort-order pres:sort-order pres:set-sort-order!)
  (sort-type pres:sort-type pres:set-sort-type!)
  (valid? pres:valid? pres:set-valid!))

(define (pres:make #!key
                   (table (table:make))
                   (filter-text #f)
                   (sort-column #f)
                   (sort-order #f)
                   (sort-type #f)
                   (valid #f))
  (%make-delectus-presentation table filter-text sort-column sort-order sort-type valid?))

