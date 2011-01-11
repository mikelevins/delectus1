;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-constants.scm
;;;; Project:       Delectus
;;;; Purpose:       constant definitions
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
(define $ERR_NO_DOCUMENT        -11)
