;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       general-purpose utility functions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;;  file utilities
;;; ---------------------------------------------------------------------

;;;  file-pathname-p (pathname)
;;; ---------------------------------------------------------------------
;;; returns true if the pathname's name or type part is nonempty
;;; does not check whether the named file actually exists

(defun file-pathname-p (pathname)
  (when pathname
    (let* ((pathname (pathname pathname))
           (name (pathname-name pathname))
           (type (pathname-type pathname)))
      (when (or (not (member name '(nil :unspecific "") :test 'equal))
                (not (member type '(nil :unspecific "") :test 'equal)))
        pathname))))

;;; ---------------------------------------------------------------------
;;;  time utilities
;;; ---------------------------------------------------------------------


;;; now-timestamp
;;; ---------------------------------------------------------------------

(defun now-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))

;;; (now-timestamp)
