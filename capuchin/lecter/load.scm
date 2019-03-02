;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       lecter
;;;; Purpose:       lecter loader for interactive testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the lecter sources are at another pathname

(define $engine-root "/Users/mikel/Workshop/src/delectus/capuchin/engine/")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '("scm/lib/uuid.scm"
    "scm/base/constants.scm"
    "scm/lib/lists.scm"
    "scm/lib/vectors.scm"
    "scm/lib/strings.scm"
    "scm/lib/Sort.scm"
    "scm/lib/sort-keys.scm"
    "scm/lib/filter-keys.scm"
    "scm/lib/functions.scm"
    "scm/data/entries.scm"
    "scm/data/rows.scm"
    "scm/data/columns.scm"
    "scm/data/tables.scm"
    "scm/data/views.scm"
    "scm/data/registry.scm"
    "scm/io/csv.scm"
    "scm/io/io-formats.scm"
    "scm/io/io.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-lecter)
  (for-each (lambda (f)
              (let ((p (string-append $engine-root f)))
                (load p)))
            $load-files))

;;; (load-lecter)
;;; (define $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (define $id (read-delectus-file $movies-path))
;;; (delectus-table? (reg:find-table $id))
;;; 
;;; (define $cartoons-path "/Users/mikel/Workshop/src/delectus/test-data/cartoons.csv")
;;; (define $id2 (read-csv-file $cartoons-path))
;;; (reg:find-table $id2)
