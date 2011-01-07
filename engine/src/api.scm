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

(define $SORT_NONE         0)
(define $SORT_DESCENDING   1)
(define $SORT_ASCENDING    2)
(define $SORT_NUMERIC      3)
(define $SORT_ALPHABETICAL 4)

(define $VAL_NO #f)
(define $VAL_YES #t)
(define $VAL_NO_VALUE #f)

(define $OBJ_NO_OID 0)

(define $ERR_NO_ERROR            0)
(define $ERR_UNKNOWN_ERROR      -1)
(define $ERR_CANT_CREATE        -2)
(define $ERR_CANT_ADD_ROW       -3)
(define $ERR_CANT_ADD_COLUMN    -4)
(define $ERR_NO_SUCH_COLUMN     -5)
(define $ERR_INDEX_OUT_OF_RANGE -6)
(define $ERR_CANT_UPDATE        -7)
(define $ERR_CANT_WRITE         -8)
(define $ERR_CANT_READ          -9)
(define $ERR_BAD_FORMAT         -10)

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version)
  0)

(define (api:make-table)
  $VAL_NO_VALUE)

(define (api:add-row oid)
  $ERR_NO_ERROR)

(define (api:add-column oid label)
  $ERR_NO_ERROR)

(define (api:value-at oid column-label row-index)
  $VAL_NO_VALUE)

(define (api:put-value-at! oid column-label row-index val)
  $ERR_NO_ERROR)

(define (api:mark-column-deleted! oid column-label deleted?)
  $ERR_NO_ERROR)

(define (api:mark-row-deleted! oid row-index deleted?)
  $ERR_NO_ERROR)

(define (api:show-deleted? oid)
  $VAL_NO)

(define (api:set-show-deleted! oid deleted?)
  $ERR_NO_ERROR)

(define (api:compact-table! oid)
  $ERR_NO_ERROR)

(define (api:sort-column oid)
  $VAL_NO_VALUE)

(define (api:set-sort-column! oid column-label)
  $ERR_NO_ERROR)

(define (api:sort-order oid)
  $SORT_NONE)

(define (api:set-sort-order! oid order)
  $ERR_NO_ERROR)

(define (api:sort-type oid)
  $SORT_NONE)

(define (api:set-sort-type! oid type)
  $ERR_NO_ERROR)

(define (api:filter-text oid)
  $VAL_NO_VALUE)

(define (api:set-filter-text! oid text)
  $ERR_NO_ERROR)

(define (api:write-delectus oid path)
  $ERR_NO_ERROR)

(define (api:read-delectus path)
  $OBJ_NO_OID)

(define (api:write-delectus/csv oid path)
  $ERR_NO_ERROR)

(define (api:read-delectus/csv path)
  $OBJ_NO_OID)


