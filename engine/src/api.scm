;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.scm
;;;; Project:       Delectus
;;;; Purpose:       the delectus api
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------

(define $NONE         0)
(define $DESCENDING   1)
(define $ASCENDING    2)
(define $NUMERIC      3)
(define $ALPHABETICAL 4)

(define $NO-VALUE 0)
(define $NO-DELECTUS 0)

(define $CANT_CREATE        -1)
(define $CANT_ADD_ROW       -2)
(define $LABEL_IN_USE       -3)
(define $NO_SUCH_COLUMN     -4)
(define $INDEX_OUT_OF_RANGE -5)
(define $CANT_UPDATE        -6)
(define $CANT_WRITE         -7)
(define $CANT_READ          -8)
(define $BAD_FORMAT         -9)

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version)
  (current-delectus-format-version))

(define (api:make-delectus)
  (new-delectus-view))

(define (api:add-row! del)
  0)

(define (api:add-column! del label)
  0)

(define (api:value-at del label index)
  "")

(define (api:put-value-at! del label index val)
  0)

(define (api:mark-column-deleted! del label deleted?)
  0)

(define (api:mark-row-deleted! del index deleted?)
  0)

(define (api:show-deleted del)
  #f)

(define (api:set-show-deleted! del deleted?)
  0)

(define (api:compact-delectus! del)
  0)

(define (api:sort-column del)
  "")

(define (api:set-sort-column! del label)
  0)

(define (api:sort-order del)
  0)

(define (api:set-sort-order! del order)
  0)

(define (api:sort-type del)
  0)

(define (api:set-sort-type! del type)
  0)

(define (api:filter-text del)
  "")

(define (api:set-filter-text! del text)
  0)

(define (api:write-delectus del)
  0)

(define (api:read-delectus del)
  $NO-DELECTUS)

(define (api:write-delectus/csv del)
  0)

(define (api:read-delectus/csv del)
  $NO-DELECTUS)

