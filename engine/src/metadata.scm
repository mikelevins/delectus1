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

(define (meta:get m key #!key (default #f))
  (let ((entry (assq key (delectus-metadata-entries m))))
    (if entry
        (cdr entry)
        default)))

(define (meta:set! m key val)
  (let ((entry (assq key (delectus-metadata-entries m))))
    (if entry
        (begin
          (set-cdr! entry val)
          val)
        (begin
          (delectus-metadata-entries-set! m (cons (cons key val)
                                                  (delectus-metadata-entries m)))
          val))))

;;; (define $m (parse-metadata '(name: "Fred" size: large)))
;;; (meta:get $m name:)
;;; (meta:get $m shape: default: 'none)
;;; (meta:set! $m shape: 'trapezoid)
;;; (meta:get $m shape: default: 'none)
;;; $m

