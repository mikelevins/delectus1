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
(define $test-data "/Users/mikel/Projects/delectus/test-data/")

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
    "lib/Sort.scm"
    "src/lists.scm"
    "src/sequences.scm"
    "src/vectors.scm"
    "src/strings.scm"
    "src/constants.scm"
    "src/values.scm"
    "src/functions.scm"
    "src/columns.scm"
    "src/rows.scm"
    "src/tables.scm"
    "src/registry.scm"
    "src/presentations.scm"
    "src/stores.scm"
    "src/csv.scm"
    ;;"src/api.scm"
    "test/scm/test-utils.scm"
    "test/scm/tables.scm"
    "test/scm/csv.scm"
    "test/scm/tests.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))

;;; (load-delectus)
;;; (run-all-tests)