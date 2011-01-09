;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file-formats.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing delectus file formats
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; constants
;;; ----------------------------------------------------------------------

(define $delectus-format-alpha-1 0)
(define $delectus-format-alpha-2 1)
(define $delectus-format-alpha-4 2)
(define $delectus-format-beta-2 3)
(define $delectus-format-1.0 4)

(define (current-delectus-format-version) $delectus-format-1.0)

