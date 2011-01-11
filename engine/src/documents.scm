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

(define (make-document table #!key (meta #f))
  (%make-delectus-document table (parse-metadata meta)))

