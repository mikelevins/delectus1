;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       the delectus data structure
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; delectus
;;; ----------------------------------------------------------------------

(define-structure <delectus>
  roster
  deleted-columns
  deleted-rows
  sort-column sort-type sort-direction
  ;; transient data
  changed?
  filter-text
  column-cache row-cache)

(define (make-delectus columns rows)
  (make-<delectus> (make-roster columns: columns rows: rows)
                   '() 
                   '()
                   #f #f #f
                   #t
                   #f
                   '() '()))