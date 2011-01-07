;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter.scm
;;;; Project:       Delectus
;;;; Purpose:       filtering utils
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; given a vector of rows, return a vector of row indexes
;;; representing rows that contain the filter text in any
;;; element. comparison is case-insensitive

(define (filter:matching-rows rows filter-text)
  (let ((row-count (vector-length rows))
        (match-list '()))
    (let loop ((i 0))
      (if (< i row-count)
          (if (vector-contains? string-ci=?
                                (vector-ref rows i)
                                filter-text)
              (set! match-list (cons i match-list)))
          (list->vector (reverse match-list))))))