;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file-io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing delectus files
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; raw data I/O
;;; ======================================================================

(define (io:read-binary-file path)
  (let* ((in #f)
         (buffer-length (file-info-size (file-info path)))
         (data-buffer (make-u8vector buffer-length)))
    (dynamic-wind
        (lambda () (set! in (open-input-file path)))
        (lambda () (read-subu8vector data-buffer 0 buffer-length in))
        (lambda () (close-input-port in)))
    data-buffer))

;;; (define $buf (io:read-binary-file "/Users/mikel/Projects/delectus/test-data/zipcode.csv"))

(define (io:write-binary-file data path)
  (let* ((byte-count (u8vector-length data))
         (out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () (write-subu8vector data 0 byte-count out))
        (lambda () (close-output-port out)))
    dest-path))

;;; ======================================================================
;;; Native Delectus I/O
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-file path)
  (let ((file-reader (lambda ()
                       (let* ((raw (io:read-binary-file path))
                              (data (u8vector->object raw)))
                         (data->store data))))
        (error-handler (lambda (err) (error "Error: unable to read Delectus data." path))))
    (with-exception-catcher error-handler file-reader)))

;;; ----------------------------------------------------------------------
;;; writing delectus files
;;; ----------------------------------------------------------------------

(define (write-delectus-file store dest-path)
  (let ((file-writer (lambda ()
                       (let* ((data (store->data store)))
                         (io:write-binary-file data path))))
        (error-handler (lambda (err) (error "Error: unable to write Delectus data." path))))
    (with-exception-catcher error-handler file-writer)))

