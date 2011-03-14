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
            $delectus-format-1.0))

;;; (api:version)

(define (api:new-delectus)
  (if-error $OBJ_NO_OID
            (new-document)))

;;; (define $doc (api:new-delectus))

(define (api:get-view id include-deleted? sort-column sort-order filter-text)
  (if-error $OBJ_NO_OID
            (get-view id
                      description:
                      (view:description
                       include-deleted: include-deleted?
                       sort-column: sort-column
                       sort-order: sort-order
                       filter-text: filter-text))))

;;; (define $doc (api:new-delectus))
;;; (api:get-view $doc #t #f #f "w")
;;; (get-view $doc include-deleted: #t sort-column: #f sort-order: #f filter-text: "w")

(define (api:count-columns id)
  (if-error $VAL_NO
            (count-columns id)))

(define (api:count-rows id)
  (if-error $VAL_NO
            (count-rows id)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:count-columns $zipid)
;;; (api:count-rows $zipid)

(define (api:value-at id column-label row-index)
  (if-error $VAL_NO_VALUE
            (value-at id column-label row-index)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:value-at $zipid "city" 43190)

(define (api:put-value-at! id column-label row-index val)
  (if-error $ERR_CANT_UPDATE
            (put-value-at! id column-label row-index val)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:value-at $zipid "city" 0)
;;; (api:put-value-at! $zipid "city" 0 "Frostbite Falls")
;;; (api:value-at $zipid "city" 0)

(define (api:row-finished? id row-index)
  (if-error $VAL_NO
            (row-finished? id row-index)))

(define (api:mark-row-finished! id row-index finished?)
  (if-error $ERR_CANT_UPDATE
            (mark-row-finished! id row-index finished?)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:row-finished? $zipid 0)
;;; (api:mark-row-finished! $zipid 0 #f)
;;; (api:row-finished? $zipid 0)

(define (api:add-row id)
  (if-error $ERR_CANT_UPDATE
            (add-row! id)))

(define (api:add-column id column-label)
  (if-error $ERR_CANT_UPDATE
            (add-column! id column-label)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:count-columns $zipid)
;;; (api:count-rows $zipid)
;;; (api:add-column $zipid "city")
;;; (api:count-columns $zipid)
;;; (api:add-column $zipid "foobar")
;;; (api:count-columns $zipid)
;;; (api:add-row $zipid)
;;; (api:count-rows $zipid)

(define (api:column-deleted? id column-label)
  (if-error $VAL_NO
            (column-deleted? id column-label)))

(define (api:mark-column-deleted! id column-label deleted?)
  (if-error $ERR_CANT_UPDATE
            (mark-column-deleted! id column-label deleted?)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:column-deleted? $zipid "city")
;;; (api:mark-column-deleted! $zipid "city" #t)
;;; (api:column-deleted? $zipid "city")
;;; (api:mark-column-deleted! $zipid "city" #f)

(define (api:column-has-total? id column-label)
  (if-error $VAL_NO
            (column-has-total? id column-label)))

(define (api:column-total id column-label)
  (if-error $VAL_NO
            (column-total id column-label)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:column-has-total? $zipid "zip")
;;; (column-total $zipid "zip")

(define (api:row-deleted? id row-index)
  (if-error $VAL_NO
            (row-deleted? id row-index)))

(define (api:mark-row-deleted! id row-index deleted?)
  (if-error $ERR_CANT_UPDATE
            (mark-row-deleted! id row-index deleted?)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:row-deleted? $zipid 0)
;;; (mark-row-deleted! $zipid 0 #t)
;;; (api:get-view $zipid #t #f #f #f)
;;; (get-view $zipid #t #f #f #f)
;;; (api:row-deleted? $zipid 0)

(define (api:compact-delectus! id)
  (if-error $ERR_CANT_UPDATE
            (compact! id)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (api:count-columns $zipid)
;;; (api:count-rows $zipid)
;;; (mark-row-deleted! $zipid 0 #t)
;;; (mark-column-deleted! $zipid "dst" #t)
;;; (api:compact-delectus! $zipid)
;;; (api:count-columns $zipid)
;;; (api:count-rows $zipid)

(define (api:write-delectus-file id path)
  (if-error $ERR_CANT_WRITE
            $ERR_NO_ERROR))

(define (api:read-delectus-file path)
  (if-error $OBJ_NO_OID
            $OBJ_DEFAULT_OID))

(define (api:write-delectus-csv id path)
  (if-error $ERR_CANT_WRITE
            (write-csv-file (find-document id) path)))

(define (api:read-delectus-csv path)
  (if-error $OBJ_NO_OID
            (read-csv-file path)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (define $zipid (read-csv-file $zip-path))
;;; $zipid