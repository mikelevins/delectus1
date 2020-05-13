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
;;; since the Delectus epoch, which is defined as January 1, 2010 at
;;; midnight GMT--not far from the time that Delectus 1 was released.
;;;
;;; SQLite stores integers as 64-bit signed, so we have 63 bits of
;;; magnitude for future timestamps. It doesn't roll over until
;;; 2010-01-01 plus 9223372036854775807 milliseconds. That's
;;; 9223372036854775807 / (1000 * 60 * 60 * 24 * 365) years, or
;;; about 292,471,208 years before the timestamp rolls over.

(defparameter +max-sqlite-integer+ 9223372036854775807)

(defparameter +clock-sequence-multiplier+ 1000000000)

(defparameter +delectus-epoch-utc+ (encode-universal-time 0 0 0 1 1 2010 0))

(defun delectus-timestamp-now ()
  (bind ((delectus-seconds (- (get-universal-time)
                              +delectus-epoch-utc+))
         (_ignore delectus-milliseconds (truncate (get-internal-real-time) internal-time-units-per-second)))
    (+ (* delectus-seconds 1000)
       delectus-milliseconds)))
