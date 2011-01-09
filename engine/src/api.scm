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

(define (api:version)
  (current-delectus-format-version))

(define (api:make-table)
  (let* ((tbl (table:make))
         (id (register-document tbl)))
    id))

(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (lambda ()
              (let ((tbl (find-document oid)))
                (if tbl
                    (begin
                      (table:add-row! tbl)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (lambda ()
              (let ((tbl (find-document oid)))
                (if tbl
                    (begin
                      (table:add-column! tbl label)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            (lambda ()
              (let* ((tbl (find-document oid))
                     (val (table:value-at tbl (table:column-index tbl column-label) row-index)))
                (if (and tbl val)
                    val
                    $VAL_NO_VALUE)))))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((tbl (find-document oid)))
                (if (and tbl val)
                    (begin
                      (table:put-value-at! tbl (table:column-index tbl column-label) row-index val)
                      $ERR_NO_ERROR)
                    $ERR_CANT_UPDATE)))))

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


