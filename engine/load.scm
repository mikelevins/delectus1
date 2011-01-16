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


;;; testing
;;; ----------------------------------------------------------------------
;;;
;;; (load-delectus)

;;; (define $d1 (api:make-document))
;;; $d1
;;; (reg:find-document $d1)
;;; (doc:add-column! (reg:find-document $d1) "Name")
;;; (doc:add-row! (reg:find-document $d1))
;;; (doc:value-at (reg:find-document $d1) "Name" 0)
;;; (doc:put-value-at! (reg:find-document $d1) "Name" 0 "Fred")
;;; (doc:value-at (reg:find-document $d1) "Name" 0)
;;; (doc:column-deleted? (reg:find-document $d1) "Name")
;;; (doc:row-deleted? (reg:find-document $d1) 0)
;;; (doc:mark-column-deleted! (reg:find-document $d1) "Name" #t)
;;; (doc:column-deleted? (reg:find-document $d1) "Name")
;;; (doc:mark-column-deleted! (reg:find-document $d1) "Name" #f)
;;; (doc:column-deleted? (reg:find-document $d1) "Name")
;;; (doc:mark-row-deleted! (reg:find-document $d1) 0 #t)
;;; (doc:row-deleted? (reg:find-document $d1) 0)
;;; (doc:mark-row-deleted! (reg:find-document $d1) 0 #f)
;;; (doc:row-deleted? (reg:find-document $d1) 0)

