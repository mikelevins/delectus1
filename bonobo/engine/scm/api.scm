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
;;; error handler
;;; ---------------------------------------------------------------------

(define (%if-error-aux errval thunk)
  (let ((handler (lambda (err) errval)))
    (with-exception-catcher handler thunk)))

(define-macro (if-error errval . body)
  `(%if-error-aux ,errval 
                  (lambda () ,@body)))

;;;(if-error "error message" (odd?))
;;;(if-error "error message" (odd? 3))

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version) $delectus-format-1.0)

(define (api:new-delectus)
  (if-error $OBJ_NO_OID
            $OBJ_NO_OID))

(define (api:get-view oid include-deleted? sort-column sort-order filter-text)
  (if-error $OBJ_NO_OID
            $OBJ_NO_OID))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            $VAL_NO_VALUE))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            $ERR_CANT_UPDATE))

(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            $ERR_CANT_ADD_ROW))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            $ERR_CANT_ADD_COLUMN))

(define (api:mark-column-deleted! oid column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            $ERR_CANT_UPDATE))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            $ERR_CANT_UPDATE))

(define (api:compact-delectus! oid)
  (if-error $ERR_CANT_UPDATE
            $ERR_CANT_UPDATE))

(define (api:write-delectus-file oid path)
  (if-error $ERR_CANT_WRITE
            $ERR_CANT_WRITE))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            $OBJ_NO_OID))

(define (api:write-delectus/csv oid path)
  (if-error $ERR_CANT_WRITE
            $ERR_CANT_WRITE))

(define (api:read-delectus/csv path)
  (if-error $OBJ_NO_OID
            $OBJ_NO_OID))

