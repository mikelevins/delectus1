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
;;; The Delectus timestamp is an integer defined as the number of
;;; seconds since the Delectus epoch plus a clock-sequence number that
;;; rolls over at a billion. Using these values, Delectus timestamps
;;; will roll over at (delectus-timestamp-to-string
;;; +max-sqlite-integer+), which is 294177-01-09T04:00:54.000000Z --
;;; January 9th, 294,177 AD at about 4 in the morning Greenwich time.

(defparameter +max-sqlite-integer+ 9223372036854775807)

(defparameter +clock-sequence-multiplier+ 1000000)

(defparameter +delectus-epoch-string+ "2010-01-01T00:00:00+00:00")
(defparameter +delectus-epoch+ (local-time:parse-rfc3339-timestring +delectus-epoch-string+))
(defparameter +delectus-epoch-utc+ (local-time:timestamp-to-universal +delectus-epoch+))

(defun seconds-since-delectus-epoch ()
  (- (get-universal-time)
     +delectus-epoch-utc+))

(defparameter *delectus-clock-sequence-number* 0)

;;; instead of relying on microsecond timers, which are not available
;;; on all platforms, we use a clock-sequence number that rolls over
;;; at a billion--so timestamps are unique per-process as long as we
;;; don't make more than a million of them per second

(defun next-delectus-clock-sequence-number ()
  (setf *delectus-clock-sequence-number*
        (mod (1+ *delectus-clock-sequence-number*)
             +clock-sequence-multiplier+)))

(defun delectus-timestamp-now ()
  (+ (* (seconds-since-delectus-epoch)
        +clock-sequence-multiplier+)
     (next-delectus-clock-sequence-number)))

;;; (delectus-timestamp-now)

(defun delectus-timestamp-to-string (ts)
  (format nil "~A" (local-time:universal-to-timestamp (truncate ts +clock-sequence-multiplier+))))

;;; (delectus-timestamp-to-string (delectus-timestamp-now))
;;; (delectus-timestamp-to-string +max-sqlite-integer+)
