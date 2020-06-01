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
;;; The Delectus timestamp is an integer: the number of microseconds
;;; since the UTC epoch: which is defined as January 1, 1900 at
;;; midnight GMT.
;;;
;;; SQLite stores integers as 64-bit signed, so we have 63 bits of
;;; magnitude for future timestamps. It doesn't roll over until
;;; 1900-01-01 plus 9223372036854775807 microseconds, which is January
;;; 9th, 294,177 AD, at 12:54 AM.
;;;
;;; TODO: 
;;; This code should work for macOS and Linux. For Windows, I need to add
;;; a function to get system time with microsecond precision. After
;;; a little research, it looks like the Windows system call I want is
;;; GetSystemTimeAsFileTime. local-time already uses it for the
;;; allegro implementation, but not Lispworks.

(defparameter +max-sqlite-integer+ 9223372036854775807)

(defun delectus-timestamp-now ()
  (let* ((delectus-seconds (get-universal-time))
         (now (local-time:now))
         (delectus-microseconds (local-time:timestamp-microsecond now)))
    (+ (* delectus-seconds 1000000)
       delectus-microseconds)))

(defun delectus-timestamp->local-time (timestamp)
  (bind ((secs usecs (truncate timestamp 1000000)))
    (local-time:timestamp+ (local-time:universal-to-timestamp secs)
                           (* usecs 1000) :nsec)))

;;; (delectus-timestamp->local-time (delectus-timestamp-now))
