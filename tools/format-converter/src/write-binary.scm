;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          write-binary.scm
;;;; Project:       Delectus
;;;; Purpose:       writing binary data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; general utilities
;;; ======================================================================

(define (write-binary-file data path)
  (let* ((byte-count (u8vector-length data))
         (out #f)
         (out-count 0))
    (dynamic-wind
        (lambda () (set! out (open-output-file path)))
        (lambda () (set! out-count (write-subu8vector data 0 byte-count out)))
        (lambda () (close-output-port out)))
    (values out-count path)))
