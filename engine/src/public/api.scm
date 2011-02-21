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
  (if-error $ERR_CANT_CREATE
            (reg:view->id (delectus:new))))

(define (api:get-view oid include-deleted? sort-column sort-order filter-text)
  (if-error $ERR_CANT_CREATE
            (reg:view->id
             (delectus:get-view oid
                                include-deleted: include-deleted?
                                sort-column: sort-column
                                sort-order: sort-order
                                filter-text: filter-text))))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            (delectus:value-at oid column-label row-index)))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (begin
              (delectus:put-value-at! oid column-label row-index val)
              $ERR_NO_ERROR)))

(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (begin
              (delectus:add-row! oid)
              $ERR_NO_ERROR)))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (begin
              (delectus:add-column! oid label)
              $ERR_NO_ERROR)))

(define (api:mark-column-deleted! oid column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (begin
              (delectus:mark-column-deleted! oid column-label deleted?)
              $ERR_NO_ERROR)))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (begin
              (delectus:mark-row-deleted! oid row-index deleted?)
              $ERR_NO_ERROR)))

(define (api:compact-delectus! oid)
  (if-error $ERR_CANT_UPDATE
            (begin
              (delectus:compact! oid)
              $ERR_NO_ERROR)))

(define (api:write-delectus-file oid path)
  (if-error $ERR_CANT_WRITE
            (begin
              (delectus:write oid path)
              $ERR_NO_ERROR)))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            (reg:view->id (delectus:read path))))

(define (api:write-delectus/csv oid path)
  (if-error $ERR_CANT_WRITE
            (begin
              (delectus:write-csv oid path)
              $ERR_NO_ERROR)))

(define (api:read-delectus/csv path)
  (if-error $OBJ_NO_OID
            (reg:view->id (delectus:read-csv path))))

