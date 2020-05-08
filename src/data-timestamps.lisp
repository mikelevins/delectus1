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
;;; The Delectus timestamp is an integer, the number of microseconds
;;; since midnight of January 1, 2010, UTC. It's chosen because it's
;;; a round date near the time when Delectus 1 was released.

(defparameter +delectus-epoch-string+ "2010-01-01T00:00:00+00:00")

(defun delectus-epoch ()
  (local-time:parse-rfc3339-timestring +delectus-epoch-string+))

(defparameter +microseconds-per-year+ (* 1000000 60 60 24 364))
(defparameter +max-sqlite-integer+ 9223372036854775807)
(defparameter +representable-years+ (float (/ +max-sqlite-integer+ +microseconds-per-year+)))

;;; the above constants give us 293,274.7 representable years. the
;;; Delectus epoch is 1/1/2010, so the Delectus timestamp integer will
;;; roll over in 2010+293,274.7 or the fall of the year 295,284 AD

(defun delectus-now ()
  (let* ((now (local-time:now))
         (seconds (- (local-time:timestamp-to-universal now)
                     (local-time:timestamp-to-universal (delectus-epoch))))
         (microseconds (local-time:timestamp-microsecond now)))
    (+ (* seconds 1000 1000)
       microseconds)))


