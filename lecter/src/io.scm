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

(define (%concat-chunks chunk more-chunks)
  (if (null? more-chunks)
      chunk
      (let ((new-chunk (u8vector-append chunk (car more-chunks))))
        (%concat-chunks new-chunk (cdr more-chunks)))))

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
        (%concat-chunks (car chunks)(cdr chunks))
        #f)))

;;; ======================================================================
;;; Native Delectus I/O
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-file path)
  (let* ((raw (io:read-binary-file path))
         (data (u8vector->object raw))
         (tbl (if (delectus-table? data)
                  data
                  (data->table data))))
    (reg:register-delectus! tbl)))

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
    (write (car cols) out)
    (for-each (lambda (col)
                (write-char #\, out)
                (write col out))
              (cdr cols))
    (newline out)))

(define (print-columns-csv tbl)
  (let* ((cols (table:column-labels tbl)))
    (write (car cols))
    (for-each (lambda (col)
                (write-char #\,)
                (write col))
              (cdr cols))
    (newline)))

(define (write-row-csv r out)
  (let ((eltcount (vector-length (row:entries r))))
    (if (> eltcount 0)
        (begin
          (write (or (row:element r 0) "") out)
          (if (> eltcount 1)
              (let loop ((i 1))
                (if (< i eltcount)
                    (begin
                      (write-char #\, out)
                      (write (or (row:element r i) "") out)
                      (loop (+ i 1))))))))))

(define (print-row-csv r)
  (let ((eltcount (vector-length (row:entries r))))
    (if (> eltcount 0)
        (begin
          (write (or (row:element r 0) ""))
          (if (> eltcount 1)
              (let loop ((i 1))
                (if (< i eltcount)
                    (begin
                      (write-char #\,)
                      (write (or (row:element r i) ""))
                      (loop (+ i 1))))))))))

(define (write-table-csv tbl out)
  (write-columns-csv tbl out)
  (vector-for-each (lambda (r)(write-row-csv r out)(newline out))
                   (table:rows tbl)))

(define (print-table-csv tbl)
  (print-columns-csv tbl)
  (vector-for-each (lambda (r)(print-row-csv r)(newline))
                   (table:rows tbl)))

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

;;; ======================================================================
;;; delectus->csv
;;; ======================================================================

(define (delectus->csv src-path)
  (let* ((raw (io:read-binary-file src-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  data
                  (data->table data))))
    (if tbl
        (print-table-csv tbl)
        (begin (format "~%Not a Delectus 1.x file: ~s" src-path)
               (format "~%No output written.~%")
               $ERR_BAD_FORMAT))))

;;; (define $inpath "/Users/mikel/Workshop/src/delectus/delectus2csv/testdata/Movies.delectus")
;;; (define $outpath "/Users/mikel/Desktop/Movies.csv")
;;; (delectus2csv $inpath $outpath)

(define (display-usage)
  (newline)
  (display "USAGE: delectus2csv INFILE"))


(define (delectus1-pathname->delectus2-pathname src-path)
  (string-append src-path ".csv"))

;;; ======================================================================
;;; file formats
;;; ======================================================================

(define (delectus-file-format path)
  (let* ((raw (io:read-binary-file path))
         (data (u8vector->object raw)))
    (delectus-format data)))

;;; ======================================================================
;;; delectus->lisp
;;; ======================================================================

(define (columns->lisp tbl)
  (let* ((colseq (table:column-sequence tbl))
         (cols (vector->list (column-sequence:columns colseq)))
         (items (map (lambda (col)
                       (let ((label (column:label col))
                             (deleted (column:deleted? col)))
                         (if deleted
                             (list label 'DELETED)
                             (list label))))
                     cols)))
    items))

(define (rows->lisp tbl)
  (map (lambda (row)
         (let* ((deleted? (if (row:deleted? row) 'T 'NIL))
                (entries (vector->list (row:entries row)))
                (items (map (lambda (entry)(entry:value entry))
                            entries)))
           (cons deleted? items)))
       (vector->list (table:rows tbl))))

(define (table->lisp tbl)
  (let* ((cols (columns->lisp tbl))
         (rows (rows->lisp tbl)))
    (list 'COLUMNS cols
          'ROWS rows)))

(define (delectus->lisp src-path)
  (let* ((raw (io:read-binary-file src-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  data
                  (data->table data))))
    (if tbl
        (table->lisp tbl)
        (begin (format "~%Not a Delectus 1.x file: ~s" src-path)
               (format "~%No conversion performed.~%")
               $ERR_BAD_FORMAT))))


(define (%if-error-aux errval thunk)
  (let ((handler (lambda (err) errval)))
    (with-exception-catcher handler thunk)))

(define-macro (if-error errval . body)
  `(%if-error-aux ,errval 
                  (lambda () ,@body)))

(define (delectus-format-version src-path)
  (if-error "INVALID"
            (let* ((raw (io:read-binary-file src-path))
                   (data (u8vector->object raw)))
              (delectus-format data))))

;;; (define $inpath "/Users/mikel/Workshop/src/delectus/lecter/test-data/junior-movies.delectus")
;;; (define $data (delectus->lisp $inpath))

(define (file-readable? src-path)
  (and (> (string-length src-path) 0)
       (file-exists? src-path)
       (let* ((userinfo (user-info (user-name)))
              (owner (file-owner src-path))
              (group (file-group src-path))
              (mode (file-mode src-path))
              (group-can-read #b000100000)
              (others-can-read #b000000100)
              (owner-can-read #b100000000))
         (or (and (= owner (user-info-uid userinfo))
                  (not (zero? (bitwise-and mode owner-can-read))))
             (and (= group (user-info-gid userinfo))
                  (not (zero? (bitwise-and mode group-can-read))))
             (not (zero? (bitwise-and mode others-can-read)))))))

(define (error-input-not-readable src-path)
  (error (format "File not readable: ~s" src-path)))
