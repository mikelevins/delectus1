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

(defun now-utc ()
  (local-time:timestamp-to-universal (now)))

;;; (now-utc)

(defun now-string ()
  (local-time:format-rfc3339-timestring nil (now)))

;;; (now-string)

(defmethod utc->timestring ((time integer))
  (local-time:format-rfc3339-timestring nil (local-time:universal-to-timestamp time)))

;;; (utc->timestring (now-utc))
