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
            (lambda ()
              #f)))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            (lambda ()
              #f)))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))


(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (lambda ()
              #f)))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (lambda ()
              #f)))

(define (api:mark-column-deleted! oid column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:include-deleted? oid)
  (if-error $VAL_NO
            (lambda ()
              #f)))

(define (api:set-include-deleted! oid show?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:compact-delectus! oid)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (reg:find-document oid)))
                #f))))

(define (api:sort-column oid)
  (if-error $VAL_NO_VALUE
            (lambda ()
              #f)))

(define (api:set-sort-column! oid column-label)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:sort-order oid)
  (if-error $SORT_NONE
            (lambda ()
              #f)))

(define (api:set-sort-order! oid order)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:filter-text oid)
  (if-error $VAL_NO_VALUE
            (lambda ()
              #f)))

(define (api:set-filter-text! oid text)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              #f)))

(define (api:write-delectus-file oid path)
  (if-error $ERR_CANT_WRITE
            (lambda ()
              #f)))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            (lambda ()
              #f)))

(define (api:write-delectus/csv oid path)
  (if-error $ERR_CANT_WRITE
            (lambda ()
              #f)))

(define (api:read-delectus/csv path)
  (if-error $OBJ_NO_OID
            (lambda ()
              #f)))

