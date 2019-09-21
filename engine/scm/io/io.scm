;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing Delectus documents
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

;;; ======================================================================
;;; Native Delectus I/O
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-data path)
  (let* ((raw (io:read-binary-file path))
         (data (u8vector->object raw)))
    (if (delectus-table? data)
        data
        (data->table data))))

(define (read-delectus-file path)
  (reg:register-delectus! (read-delectus-data path)))

;;; (define $jr-path "/Users/mikel/Projects/delectus/delectus/test-data/junior-movies.delectus")
;;; (define $jr (read-delectus-file $jr-path))

;;; ----------------------------------------------------------------------
;;; writing delectus files
;;; ----------------------------------------------------------------------

(define (write-delectus-file tbl dest-path)
  (let* ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () 
          (let* ((bytes (object->u8vector tbl))
                 (bytecount (u8vector-length bytes)))
            (write-subu8vector bytes 0 bytecount out)))
        (lambda () (close-output-port out)))
    dest-path))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (define $ziptest-path "/Users/mikel/Desktop/ziptest.delectus")
;;; (write-delectus-file (find-document $zipid) $ziptest-path)
;;; (define $ziptest (read-delectus-file $ziptest-path))

;;; ======================================================================
;;; CSV I/O
;;; ======================================================================

(define (read-csv-file path)
  (let* ((table (csv:read path)))
    (reg:register-delectus! table)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (read-csv-file $zip-path))
;;; (document? (find-document $zipid))

;;; (define $in-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $out-path "/Users/mikel/Desktop/testdelectus.csv")
;;; (define $out-path"/private/var/folders/6r/6rDpY9CfEAOBLrIbLcACj++++TI/TemporaryItems/(A Document Being Saved By Delectus 3)/testdelectus.csv")
;;; (define $doc (read-csv-file $in-path))
;;; (api:write-delectus-csv $doc $out-path)

(define (write-columns-csv tbl out)
  (let* ((cols (table:column-labels tbl)))
    (if (null? cols)
        #f
        (begin
          (write (car cols) out)
          (for-each (lambda (col)
                      (write-char #\, out)
                      (write col out))
                    (cdr cols))
          (newline out)))))

(define (write-row-csv r out)
  (let ((eltcount (vector-length (row:entries r))))
    (if (> eltcount 0)
        (begin
          (write (row:element r 0) out)
          (if (> eltcount 1)
              (let loop ((i 1))
                (if (< i eltcount)
                    (begin
                      (write-char #\, out)
                      (write (row:element r i) out)
                      (loop (+ i 1))))))))))

(define (write-table-csv tbl out)
  (write-columns-csv tbl out)
  (let ((rows (table:rows tbl)))
    (if (> (vector-length rows) 0)
        (vector-for-each (lambda (r)(write-row-csv r out)(newline out))
                         rows)
        #f))
  (newline out))

(define (write-csv-file tbl dest-path)
  (let ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () 
          (write-table-csv tbl out))
        (lambda () (close-output-port out))))
  dest-path)

;;; (define $ziptest-path "/Users/mikel/Desktop/ziptest.csv")
;;; (write-csv-file (find-document $zipid) $ziptest-path)
