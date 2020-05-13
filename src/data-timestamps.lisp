;;;; ***********************************************************************
;;;;
;;;; Name:          data-timestamps.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus timestamps
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; The Delectus timestamp is an integer: the number of milliseconds
;;; since the UTC epoch: which is defined as January 1, 1900 at
;;; midnight GMT.
;;;
;;; SQLite stores integers as 64-bit signed, so we have 63 bits of
;;; magnitude for future timestamps. It doesn't roll over until
;;; 1900-01-01 plus 9223372036854775807 milliseconds, which is August
;;; 17th, 292,278,924 AD, at 12:55 PM.

(defparameter +max-sqlite-integer+ 9223372036854775807)

(defparameter +clock-sequence-multiplier+ 1000000000)

(defun delectus-timestamp-now ()
  (bind ((delectus-seconds (get-universal-time))
         (_ignore delectus-milliseconds (truncate (get-internal-real-time) internal-time-units-per-second)))
    (+ (* delectus-seconds 1000)
       delectus-milliseconds)))
