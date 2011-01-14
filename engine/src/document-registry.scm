;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-registry.scm
;;;; Project:       Delectus
;;;; Purpose:       identifying documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define next-document-id #f)
(define current-document-id #f)

(let ((document-id 0))
  (set! next-document-id
        (lambda ()
          (set! document-id (+ 1 document-id))
          document-id))
  (set! current-document-id
        (lambda () document-id)))

(define registry:no-document 0)
(define registry:document->id-table (make-table test: eqv?))
(define registry:id->document-table (make-table test: eqv?))

(define (find-document id)
  (table-ref registry:id->document-table id #f))

(define (document-id doc)
  (table-ref registry:document->id-table doc #f))

(define (register-document doc)
  (let ((already-id (document-id doc)))
    (or already-id
        (let ((new-id (next-document-id)))
          (table-set! registry:document->id-table doc new-id)
          (table-set! registry:id->document-table new-id doc)
          new-id))))

