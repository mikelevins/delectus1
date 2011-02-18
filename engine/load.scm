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

(define $delectus-root "/Users/mikel/Projects/delectus/")
(define $engine-root (string-append $delectus-root "engine/"))
(define $test-data-root (string-append $delectus-root "test-data/"))

;;; Scheme files to load for interactive testing
;;; ----------------------------------------------------------------------

(define $load-files
  '(
    "lib/uuid.scm"
    "lib/Sort.scm"
    "src/public/constants.scm"
    "src/public/api.scm"
    "src/private/lists.scm"
    "src/private/vectors.scm"
    "src/private/strings.scm"
    "src/private/functions.scm"
    "src/private/data.scm"
    "src/private/views.scm"
    ;;"src/private/registry.scm"
    ;;"src/private/delectus.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $engine-root f)))
                (load p)))
            $load-files))

;;; (load-delectus)
;;; (run-all-tests)
