;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          view.scm
;;;; Project:       Delectus
;;;; Purpose:       computed views of a delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-structure <view>
  ;; the delectus data
  delectus
  ;; deleted rows and columns
  deleted-columns deleted-rows
  include-deleted?
  ;; sort
  sort-column sort-direction sort-type
  ;; ephemeral data
  filter-text
  changed?)

(define (make-view )
  )




