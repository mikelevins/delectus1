;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-registry.scm
;;;; Project:       Delectus
;;;; Purpose:       keeping track of documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; per-session document ids
;;; ----------------------------------------------------------------------

(define current-document-id #f)
(define next-document-id #f)
(define $id-no-document 0)
(let ((document-id $id-no-document))
  (set! current-document-id 
        (lambda () document-id))
  (set! next-document-id 
        (lambda ()
          (set! document-id (+ document-id 1))
          document-id)))

;;; ----------------------------------------------------------------------
;;; registry tables
;;; ----------------------------------------------------------------------

(define *document->id-table* (make-table test: eqv?))
(define *id->document-table* (make-table test: eqv?))

;;; ----------------------------------------------------------------------
;;; document matching and registration
;;; ----------------------------------------------------------------------

(define (reg:find-document id)
  (table-ref *id->document-table* id #f))

(define (reg:document-id doc)
  (table-ref *document->id-table* doc #f))

(define (reg:register-document! doc id)
  (if id
      (let ((old-doc (reg:find-document id)))
        (if old-doc (table-set! *document->id-table* old-doc))
        (table-set! *id->document-table* id doc)
        (table-set! *document->id-table* doc id)
        id)
      (let ((id (next-document-id)))
        (table-set! *id->document-table* id doc)
        (table-set! *document->id-table* doc id)
        id)))



