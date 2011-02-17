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

(define (if-error errval thunk)
  (let ((handler (lambda (err) errval)))
    (with-exception-catcher handler thunk)))

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------


(define (api:version) $delectus-format-1.0)

(define (api:new-delectus)
  (if-error $ERR_CANT_CREATE
            (lambda ()(reg:new-delectus))))

(define (api:view oid include-deleted? sort-column sort-order filter-text)
  (if-error $ERR_CANT_CREATE
            (lambda ()
              (reg:get-view (reg:get oid)
                            include-deleted: include-deleted?
                            sort-column: sort-column
                            sort-order: sort-order
                            filter-text: filter-text))))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            (lambda ()(delectus:value-at (reg:get oid) column-label row-index))))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (lambda ()(delectus:put-value-at! (reg:get oid) column-label row-index))))


(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (lambda ()(delectus:add-row! (reg:get oid)))))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (lambda ()(delectus:add-column! (reg:get oid)))))

(define (api:mark-column-deleted! oid column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()(delectus:mark-column-deleted! (reg:get oid) column-label deleted?))))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()(delectus:mark-row-deleted! (reg:get oid) row-index deleted?))))

(define (api:compact-delectus! oid)
  (if-error $ERR_CANT_UPDATE
            (lambda ()(delectus:compact! (reg:get oid)))))

(define (api:write-delectus-file oid path)
  (if-error $ERR_CANT_WRITE
            (lambda ()(delectus:write (reg:get oid) path))))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            (lambda ()(delectus:read path))))

(define (api:write-delectus/csv oid path)
  (if-error $ERR_CANT_WRITE
            (lambda ()(delectus:write-csv (reg:get oid) path))))

(define (api:read-delectus/csv path)
  (if-error $OBJ_NO_OID
            (lambda ()(delectus:read-csv path))))

