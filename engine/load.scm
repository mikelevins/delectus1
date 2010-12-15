;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus
;;;; Purpose:       repl loader for delectus engine
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

(define $engine-load-files
  '("src/lists.scm"
    "src/vectors.scm"
    "src/sort.scm"
    "src/versions.scm"
    "src/delectus.scm"
    "src/csv.scm"))

(define (load-engine)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $engine-load-files))

;;; (load-engine)