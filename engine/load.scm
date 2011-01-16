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
  '(
    "lib/uuid.scm"
    "src/lists.scm"
    "src/maps.scm"
    "src/sequences.scm"
    "src/vectors.scm"
    "src/constants.scm"
    "src/values.scm"
    "src/tables.scm"
    "src/views.scm"
    "src/documents.scm"
    "src/document-registry.scm"
    "src/fileio.scm"
    "src/csv.scm"
    "src/api.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))


