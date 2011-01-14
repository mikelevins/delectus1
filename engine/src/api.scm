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

(define (api:make-document)
  (new-document))

(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (lambda ()
              (let ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:add-row! doc)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (lambda ()
              (let ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:add-column! doc label)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:value-at oid column-label row-index)
  (if-error $VAL_NO_VALUE
            (lambda ()
              (let* ((doc (find-document oid))
                     (val (doc:value-at doc (doc:column-index doc column-label) row-index)))
                (if (and doc val)
                    val
                    $VAL_NO_VALUE)))))

(define (api:put-value-at! oid column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if (and doc val)
                    (begin
                      (doc:put-value-at! doc (doc:column-index doc column-label) row-index val)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:mark-column-deleted! oid column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:mark-column-deleted! doc column-label deleted?)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:mark-row-deleted! doc row-index deleted?)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:show-deleted? oid)
  (if-error $VAL_NO
            (lambda ()
              (let* ((doc (find-document oid)))
                (if (and doc (doc:show-deleted? doc))
                    $VAL_YES
                    $VAL_NO)))))

(define (api:set-show-deleted! oid deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:set-show-deleted! doc show?)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

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


;;; (define $doc (new-document))
;;; (api:add-column! $doc "Name")
;;; (api:add-row! $doc)
;;; (api:mark-column-deleted! $doc "Name" #t)
;;; (define $d (find-document $doc))
;;; (doc:set-meta! $d (cons "Name" (doc:get-meta $d )))