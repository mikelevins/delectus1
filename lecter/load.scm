;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus
;;;; Purpose:       loader for interactive testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the Delectus sources are at another pathname

(define $delectus-root "/Users/mikel/Workshop/src/delectus/lecter")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '("/src/uuid.scm"
    "/src/constants.scm"
    "/src/lists.scm"
    "/src/vectors.scm"
    "/src/strings.scm"
    "/src/Sort.scm"
    "/src/sort-keys.scm"
    "/src/filter-keys.scm"
    "/src/functions.scm"
    "/src/entries.scm"
    "/src/rows.scm"
    "/src/columns.scm"
    "/src/tables.scm"
    "/src/views.scm"
    "/src/registry.scm"
    "/src/csv.scm"
    "/src/io-formats.scm"
    "/src/io.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $delectus-root f)))
                (load p)))
            $load-files))

;;; (load-delectus)
