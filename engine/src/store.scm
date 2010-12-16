;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store.scm
;;;; Project:       Delectus
;;;; Purpose:       i/o for delectus data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; general utilities
;;; ======================================================================

(define (io:read-binary-file path)
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
              ((< byte-count buffer-length) (set! chunks (append chunks (list (subu8vector data-buffer 0 byte-count)))))
            (set! chunks (append chunks (list (subu8vector data-buffer 0 byte-count))))))
        (lambda () (close-input-port in)))
    ;; convert the bytes to a scheme object and return it
    (if (not (null? chunks))
        (apply u8vector-append chunks)
        #f)))

(define (store:vector-format? x tag)
  (and (vector? x)
       (> (vector-length x) 1)
       (eqv? tag (vector-ref x 0))))

(define (serialized-store? s)(store:vector-format? s 'store))

;;; ======================================================================
;;; serializing delectus data for storage
;;; ======================================================================

(define (serialize-delectus del)
  (object->u8vector del))

(define (converter-for-format data)
  (table-ref $data-converters (delectus-data-format data) #f))

(define (data->delectus data)
  (let ((converter (converter-for-format data)))
    (converter data)))

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-file path)
  (let ((file-reader (lambda ()
                       (let* ((raw (io:read-binary-file path))
                              (data (u8vector->object raw)))
                         (data->delectus data))))
        (error-handler (lambda (err)
                         (newline)
                         (display "error reading delectus file")
                         (newline)
                         (display err)
                         (newline)
                         err)))
    (with-exception-catcher error-handler file-reader)))

;;; ----------------------------------------------------------------------
;;; writing delectus files
;;; ----------------------------------------------------------------------

(define (write-delectus-file del dest-path)
  (let* ((doc-bytes (serialize-delectus del))
         (byte-count (u8vector-length doc-bytes))
         (out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () (write-subu8vector doc-bytes 0 byte-count out))
        (lambda () (close-output-port out)))
    dest-path))


;;; (time (write-delectus-file $d "/Users/mikel/Desktop/zipcode.delectus"))
;;; (define $d2)
;;; (time (set! $d2 (read-delectus-file "/Users/mikel/Desktop/zipcode.delectus")))