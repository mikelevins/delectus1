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

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version)
  (if-error $VAL_NO_VALUE
            "1.0"))

(define (api:new-delectus)
  (if-error $OBJ_NO_OID
            (new-document)))

(define (api:get-view id include-deleted? sort-column sort-order filter-text)
  (if-error $OBJ_NO_OID
            (get-view id
                      description:
                      (view:description
                       include-deleted: include-deleted?
                       sort-column: sort-column
                       sort-order: sort-order
                       filter-text: filter-text))))

(define (api:count-columns id)
  (if-error 0
            (count-columns id)))

(define (api:sort-column id)
  (if-error $VAL_NO_VALUE
            (sort-column id)))

(define (api:sort-order id)
  (if-error $SORT_NONE
            (sort-order id)))

(define (api:include-deleted? id)
  (if-error $VAL_NO
            (include-deleted? id)))

(define (api:filter-text id)
  (if-error $VAL_NO_VALUE
            (filter-text id)))

(define (api:column-at-index id index)
  (if-error $OBJ_NO_OID
            (column-at-index id index)))

(define (api:count-rows id)
  (if-error 0
            (count-rows id)))

(define (api:value-at id column-label row-index)
  (if-error $VAL_NO_VALUE
            (value-at id column-label row-index)))

(define (api:put-value-at! id column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (begin
              (put-value-at! id column-label row-index val)
              $ERR_NO_ERROR)))

(define (api:row-finished? id row-index)
  (if-error $VAL_NO
            (row-finished? id row-index)))

(define (api:mark-row-finished! id row-index finished?)
  (if-error $ERR_CANT_UPDATE
            (begin
              (mark-row-finished! id row-index finished?)
              $ERR_NO_ERROR)))

(define (api:add-row id)
  (if-error $ERR_CANT_UPDATE
            (begin
              (add-row! id)
              $ERR_NO_ERROR)))

(define (api:add-column id column-label)
  (if-error $ERR_CANT_UPDATE
            (begin
              (add-column! id column-label)
              $ERR_NO_ERROR)))

(define (api:column-deleted? id column-label)
  (if-error $VAL_NO
            (column-deleted? id column-label)))

(define (api:duplicate-label? id column-label)
  (if-error $VAL_YES
            (duplicate-label? id column-label)))

(define (api:mark-column-deleted! id column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (begin
              (mark-column-deleted! id column-label deleted?)
              $ERR_NO_ERROR)))

(define (api:column-has-total? id column-label)
  (if-error $VAL_NO
            (column-has-total? id column-label)))

(define (api:column-total id column-label)
  (if-error 0.0
            (column-total id column-label)))

(define (api:row-deleted? id row-index)
  (if-error $VAL_NO
            (row-deleted? id row-index)))

(define (api:mark-row-deleted! id row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (begin
              (mark-row-deleted! id row-index deleted?)
              $ERR_NO_ERROR)))

(define (api:compact-delectus! id)
  (if-error $ERR_CANT_UPDATE
            (begin
              (compact! id)
              $ERR_NO_ERROR)))

(define (api:write-delectus-file id path)
  (if-error $ERR_CANT_WRITE
            (begin
              (write-delectus-file (find-document id) path)
              $ERR_NO_ERROR)))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            (read-delectus-file path)))

(define (api:write-delectus-csv id path)
  (if-error $ERR_CANT_WRITE
            (begin
              (write-csv-file (find-document id) path)
              $ERR_NO_ERROR)))

(define (api:read-delectus-csv path)
  (if-error $OBJ_NO_OID
            (read-csv-file path)))

