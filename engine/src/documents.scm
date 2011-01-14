;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          documents.scm
;;;; Project:       Delectus
;;;; Purpose:       presenting delectus tables as a document
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type delectus-document
  id: DB7F49A3-BBF6-40F3-95A1-038CCACE1D88
  constructor: %make-delectus-document
  table
  metadata)

(define (make-document table #!key (meta '()))
  (%make-delectus-document table (parse-metadata meta)))

(define (new-document)
  (let* ((tbl (table:make))
         (doc (make-document tbl)))
    (register-document doc)))

(define (doc:table doc)
  (delectus-document-table doc))

(define (doc:get-meta doc key #!key (default #f))
  (meta:get (delectus-document-metadata doc) key default: default))

(define (doc:set-meta! doc key val)
  (meta:set! (delectus-document-metadata doc) key val))

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (doc:column-index doc column-label)
  (table:column-index (doc:table doc) column-label))

(define (doc:add-row! doc)
  (table:add-row! (doc:table doc)))

(define (doc:add-column! doc label)
  (table:add-column! (doc:table doc) label))

(define (doc:value-at doc column-index row-index)
  (table:value-at (doc:table doc) column-index row-index))

(define (doc:put-value-at! doc column-index row-index val)
  (table:put-value-at! (doc:table doc) column-index row-index val))

(define (doc:mark-column-deleted! doc column-label deleted?)
  (let ((deleted-cols (doc:get-meta doc 'deleted-columns default: '())))
    (if deleted?
        (doc:set-meta! doc 'deleted-columns (cons column-label deleted-cols))
        (doc:set-meta! doc 'deleted-columns (remove column-label deleted-cols test: string-ci=?)))))

(define (doc:mark-row-deleted! doc row-index deleted?)
  (let ((deleted-rows (doc:get-meta doc 'deleted-rows default: '())))
    (if deleted?
        (doc:set-meta! doc 'deleted-rows (cons row-index deleted-rows))
        (doc:set-meta! doc 'deleted-rows (remove row-index deleted-rows test: =)))))

(define (doc:show-deleted? doc)
  (doc:get-meta doc 'show-deleted? default: #f))

(define (doc:set-show-deleted! doc show?)
  (doc:set-meta! doc 'show-deleted? show?))


