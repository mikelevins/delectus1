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

(define $root "/Users/mikel/Valise/bywicket/Delectus")

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; This file loads the pure-Scheme portion of Delectus, which is
;;; nearly everything except the Cocoa UI.

;;; Scheme files to load for interactive testing
;;; ----------------------------------------------------------------------

(define $load-files
  '(;; infrastructure
    "/lib/scm/csv.scm"
    "/lib/scm/Sort.scm"
    "/lib/srfi/srfi1.scm"
    "/lib/srfi/srfi13.scm"
    "/lib/srfi/srfi28.scm"
    "/scm/errors.scm"
    "/scm/utils.scm"
    "/scm/csv-utils.scm"
    ;; data management
    "/scm/store.scm"
    "/scm/store-io.scm"
    "/scm/document.scm"
    ;; tests
    "/scm/test-utils.scm"
    "/scm/store-tests.scm"
    "/scm/store-io-tests.scm"
    "/scm/document-tests.scm"
    "/scm/tests.scm"
    ))

;;; Cocoa files
;;; ----------------------------------------------------------------------
;;; these are the Scheme sources for code that interacts with
;;; Objective-C and Cocoa. We can't load these interactively, because
;;; they depend on interaction with the NSApplication runtime,
;;; which is available only in the compiled application. For that 
;;; reason, code in these files is kept as simple as possible
;;;
;;; Cocoa.scm
;;; Cocoa-cache.scm
;;; c-interface.scm
;;; main.scm
;;;

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $root f)))
                (load p)))
            $load-files))