;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       data engine
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Data Registry
;;; ======================================================================

(define *oid->data-table* (make-table))

;;; ======================================================================
;;; Data Structures
;;; ======================================================================

;;; the record pointed to by an oid
;;; all API calls operate on this
;;; it contains references to the delectus and to
;;; the current computed view, if any
(define-type delectus-record
  id: F956C5ED-8EF2-4C35-8E37-794EDF4381D0
  constructor: make-delectus-record
  delectus
  view)

;;; the base data and user settings
(define-type delectus
  id: 8A68D921-4553-4F94-BC60-9D6624A123AD
  constructor: make-delectus
  columns
  rows
  sort-column
  sort-order
  filter-text
  filename
  include-deleted?)

;;; a computed view of the data, filtered and sorted
;;; according to user settings
(define-type delectus-view
  id: 045F1C7B-788D-4F47-A325-BDB0D60EF205
  constructor: make-delectus-view
  columns
  rows)

;;; the data prepared for writing to disk.
;;; on output we build one of these and write it to disk
;;; on input we read one of these and use t to construct the delectus
(define-type delectus-store
  id: BE375B74-4FF8-48D7-B01D-A0EE4228CE7B
  constructor: make-delectus-store
  format-version
  columns
  rows
  sort-column
  sort-order
  include-deleted?)

