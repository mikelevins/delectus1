;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus
;;;; Purpose:       system loader for engine development and testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the Delectus sources are at another pathname

(define $delectus-root "/Users/mikel/Projects/delectus/delectus/bonobo/engine/scm/")
(define $test-data-root "/Users/mikel/Projects/delectus/delectus/test-data/")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '(
    "uuid.scm"
    "constants.scm"
    "lists.scm"
    "vectors.scm"
    "strings.scm"
    "Sort.scm"
    "functions.scm"
    "api.scm"
    "data.scm"
    "views.scm"
    "documents.scm"
    "csv.scm"
    "io-formats.scm"
    "io.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $delectus-root f)))
                (load p)))
            $load-files))

;;; (load-delectus)
