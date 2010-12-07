;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (import-csv-file pathname headers-in-first-line?)
  (let ((store (read-csv-file pathname headers-in-first-line?)))
    (if store
        (let ((doc (document store)))
          (register-document! (next-document-id) doc))
        #f)))

(define (load-delectus-file pathname)
  (let ((store (read-delectus-file pathname)))
    (if store
        (let ((doc (document store)))
          (register-document! (next-document-id) doc))
        #f)))

(define (export-csv-file docID dest)
  (let ((doc (get-document docID)))
    (if doc
        (begin
          (write-csv-file (document.store doc) dest)
          docID)
        #f)))

(define (save-delectus-file docID dest)
  (let ((doc (get-document docID)))
    (if doc
        (begin
          (write-delectus-file (document.store doc) dest)
          docID)
        #f)))