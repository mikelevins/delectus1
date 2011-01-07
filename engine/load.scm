;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus
;;;; Purpose:       project loader for repl testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the Delectus sources are at another pathname

(define $root "/Users/mikel/Projects/delectus/engine/")

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; This file loads the pure-Scheme portion of Delectus, which is
;;; nearly everything except the Cocoa UI.

;;; Scheme files to load for interactive testing
;;; ----------------------------------------------------------------------

(define $load-files
  '("lib/uuid.scm"
    "lib/Sort.scm"
    "src/lists.scm"
    "src/vectors.scm"
    "src/functions.scm"
    "src/sequences.scm"
    "src/maps.scm"
    "src/delectus-values.scm"
    "src/delectus-tables.scm"
    "src/delectus-views.scm"
    "src/filter.scm"
    "src/sort.scm"
    "src/delectus-documents.scm"
    "src/document-registry.scm"
    "src/file-formats.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))

;;; (load-delectus)