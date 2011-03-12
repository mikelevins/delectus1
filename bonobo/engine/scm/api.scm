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

(define (api:version)
  (if-error $VAL_NO_VALUE
            $VAL_DEFAULT_VALUE))

(define (api:new-delectus)
  (if-error $OBJ_NO_OID
            $OBJ_DEFAULT_OID))

(define (api:get-view id include-deleted? sort-column sort-order filter-text)
  (if-error $OBJ_NO_OID
            $OBJ_DEFAULT_OID))

(define (api:value-at id column-label row-index)
  (if-error $VAL_NO_VALUE
            $VAL_DEFAULT_VALUE))

(define (api:put-value-at! id column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            $ERR_NO_ERROR))

(define (api:add-row id)
  (if-error $ERR_CANT_UPDATE
            $ERR_NO_ERROR))

(define (api:add-column! id column-label)
  (if-error $ERR_CANT_UPDATE
            $ERR_NO_ERROR))

(define (api:mark-row-deleted! id row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            $ERR_NO_ERROR))

(define (api:compact-delectus! id)
  (if-error $ERR_CANT_UPDATE
            $ERR_NO_ERROR))

(define (api:write-delectus-file id path)
  (if-error $ERR_CANT_WRITE
            $ERR_NO_ERROR))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            $OBJ_DEFAULT_OID))

(define (api:write-delectus-csv id path)
  (if-error $ERR_CANT_WRITE
            $ERR_NO_ERROR))

(define (api:read-delectus-csv path)
  (if-error $OBJ_NO_OID
            $OBJ_DEFAULT_OID))
