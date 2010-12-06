;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus/libdelectus
;;;; Purpose:       loader for repl testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; libdelectus packages the Delectus data engine as a library

;;; modify if the Delectus sources are at another pathname

(define $root "/Users/mikel/Projects/delectus/src/libdelectus/")

;;; Scheme files to load for interactive testing
;;; ----------------------------------------------------------------------

(define $load-files
  '("model/delectus.scm"
    ))

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))

;;; (load-delectus)