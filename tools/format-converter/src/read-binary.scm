;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read-binary.scm
;;;; Project:       Delectus
;;;; Purpose:       reading binary data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; general utilities
;;; ======================================================================

(define (read-binary-file path)
  (let* ((in #f)
         (data-bytes #f)
         (chunks (list))
         (buffer-length 4096)
         (data-buffer (make-u8vector buffer-length 0)))
    (dynamic-wind
        (lambda () (set! in (open-input-file path)))
        (lambda ()
          ;; read bytes from the file
          (do ((byte-count (read-subu8vector data-buffer 0 buffer-length in)
                           (read-subu8vector data-buffer 0 buffer-length in)))
              ((< byte-count buffer-length)
               (set! chunks (cons (subu8vector data-buffer 0 byte-count) chunks)))
            (set! chunks (cons (subu8vector data-buffer 0 byte-count) chunks))))
        (lambda () (close-input-port in)))
    ;; consolidate the bytes into a single bytevector and return it
    (if (not (null? chunks))
        (apply u8vector-append (reverse chunks))
        #f)))

