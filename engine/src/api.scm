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
  $delectus-format-1.0)

(define (api:make-document)
  (reg:register-document! (doc:make table: (table:make)) #f))

(define (api:add-column! oid label)
  (if-error $ERR_CANT_ADD_COLUMN
            (lambda ()
              (let ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:add-column! doc label)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:add-row! oid)
  (if-error $ERR_CANT_ADD_ROW
            (lambda ()
              (let ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:add-row! doc)
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

(define (api:column-deleted? oid column-label)
  (if-error $VAL_NO
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (if (doc:column-deleted? column-label)
                        $VAL_YES
                        $VAL_NO)
                    $VAL_NO)))))

(define (api:mark-row-deleted! oid row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:mark-row-deleted! doc row-index deleted?)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:row-deleted? oid row-index)
  (if-error $VAL_NO
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (if (doc:row-deleted? row-index)
                        $VAL_YES
                        $VAL_NO)
                    $VAL_NO)))))

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
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:compact-table! doc)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:sort-column oid)
  (if-error $VAL_NO_VALUE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (or (doc:sort-column doc)
                        $VAL_NO_VALUE)
                    $VAL_NO_VALUE)))))

(define (api:set-sort-column! oid column-label)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:set-sort-column! doc column-label)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:sort-order oid)
  (if-error $SORT_NONE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (or (doc:sort-order doc)
                        $SORT_NONE)
                    $SORT_NONE)))))

(define (api:set-sort-order! oid order)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:set-sort-order! doc order)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:sort-type oid)
  (if-error $SORT_NONE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (or (doc:sort-type doc)
                        $SORT_NONE)
                    $SORT_NONE)))))

(define (api:set-sort-type! oid type)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:set-sort-type! doc type)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

(define (api:filter-text oid)
  (if-error $VAL_NO_VALUE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (or (doc:filter-text doc)
                        $VAL_NO_VALUE)
                    $VAL_NO_VALUE)))))

(define (api:set-filter-text! oid text)
  (if-error $ERR_CANT_UPDATE
            (lambda ()
              (let* ((doc (find-document oid)))
                (if doc
                    (begin
                      (doc:set-filter-text! doc text)
                      $ERR_NO_ERROR)
                    $ERR_NO_DOCUMENT)))))

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