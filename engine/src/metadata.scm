;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          metadata.scm
;;;; Project:       Delectus
;;;; Purpose:       storing metadata for delectus artifacts
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type delectus-metadata
  id: 3BF78C0F-ADFF-4C8B-B14A-DA062F8EBF40
  constructor: %make-metadata
  entries)

(define (parse-metadata meta)
  (%make-metadata (plist->alist meta)))



