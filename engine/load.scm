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
    "lib/wt-tree.scm"
    "src/constants.scm"
    "src/api.scm"
    "src/lists.scm"
    "src/delectus.scm"
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