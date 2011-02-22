;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store-io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing Delectus stores
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
;;; Delectus file formats
;;; ----------------------------------------------------------------------

;;; serialized data formats

(define (io:alpha-1-format? data)(store:vector-format? data '<store>))
(define (io:alpha-2-format? data)
  (and (store:vector-format? data 'store)
       (= 4 (vector-length data))))

(define (io:alpha-4-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-alpha-4)))

(define (io:beta-2-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-beta-2)))

(define (delectus-format data)
  (cond
   ((io:alpha-1-format? data) $delectus-format-alpha-1)
   ((io:alpha-2-format? data) $delectus-format-alpha-2)
   ((io:alpha-4-format? data) $delectus-format-alpha-4)
   ((io:beta-2-format? data) $delectus-format-beta-2)
   (else #f)))

;;; ----------------------------------------------------------------------
;;; converting delectus formats
;;; ----------------------------------------------------------------------

(define $data-converters (make-table))

(define (io:from-format-alpha-1 data)
  (store:make
   version: (current-store-format)     ; version
   columns: (map (lambda (c)           ; columns
                   (make-column
                    (vector-ref c 2) ; label
                    (vector-ref c 1))) 
                 (vector-ref data 1))
   show-deleted: #f
   sort-column: #f ; sort-column
   sort-reversed: #f ; sort-reversed?
   rows: (map (lambda (r)           ; rows
                (make-row
                 (map make-field (vector-ref r 2)) ; fields
                 (vector-ref r 1))) 
              (vector-ref data 2))
   notes: #f))

(define (io:from-format-alpha-2 data)
  (store:make
   version: (current-store-format)     ; version
   columns: (map (lambda (c)           ; columns
                   (make-column
                    (vector-ref c 1) ; label
                    (vector-ref c 2))) 
                 (vector-ref data 2))
   show-deleted: #f ; show-deleted?
   column-layout: '() ; column-layout
   window-layout: '() ; window-layout
   sort-column: #f
   sort-reversed: #f
   rows: (map (lambda (r)           ; rows
                (make-row
                 (map (lambda (f) (make-field (vector-ref f 1))) ; fields
                      (vector-ref r 1))
                 (vector-ref r 2))) 
              (vector-ref data 6))
   notes: (vector-ref data 7)))

(define (io:from-format-alpha-4 data) 
  (store:make
   version: (current-store-format)
   columns: (map (lambda (c)
                   (make-column
                    (vector-ref c 1) ; label
                    (vector-ref c 3))) 
                 (vector-ref data 2))
   show-deleted: #f
   column-layout: '()
   window-layout: '()
   sort-column: #f
   sort-reversed: #f
   rows: (map (lambda (r)
                (make-row
                 (map (lambda (f) (make-field (vector-ref f 1))) ; fields
                      (vector-ref r 1))
                 (vector-ref r 2))) 
              (vector-ref data 6))
   notes: (vector-ref data 7)))

(define (io:from-format-beta-2 data) (store.deserialize data))

(table-set! $data-converters $delectus-format-alpha-1 io:from-format-alpha-1)
(table-set! $data-converters $delectus-format-alpha-2 io:from-format-alpha-2)
(table-set! $data-converters $delectus-format-alpha-4 io:from-format-alpha-4)
(table-set! $data-converters $delectus-format-beta-2 io:from-format-beta-2)

(define (converter-for-format data)
  (table-ref $data-converters (delectus-format data) #f))

(define (data->store data)
  (let ((converter (converter-for-format data)))
    (converter data)))

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-file path)
  (let ((file-reader (lambda ()
                       (let* ((raw (io:read-binary-file path))
                              (data (u8vector->object raw)))
                         (data->store data))))
        (error-handler (lambda (err)
                         (report-error context: (format "(read-delectus-file \"~a\")" path)
                                       error: err
                                       message: (format "Error reading the file '~a'; unable to read Delectus data." path))
                         #f)))
    (with-exception-catcher error-handler file-reader)))

;;; ----------------------------------------------------------------------
;;; writing delectus files
;;; ----------------------------------------------------------------------

(define (write-delectus-file store dest-path)
  (let* ((ser-store (store.serialize store))
         (doc-bytes (object->u8vector ser-store))
         (byte-count (u8vector-length doc-bytes))
         (out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () (write-subu8vector doc-bytes 0 byte-count out))
        (lambda () (close-output-port out)))
    dest-path))

;;; ======================================================================
;;; CSV I/O
;;; ======================================================================

(define (read-csv-file path #!optional (headers-in-first-line? #t))
  (let* ((data (csv:read-csv-file path)))
    (if data
        (let ((labels (car data))
              (row-data (cdr data)))
          (if (contains-duplicates? labels string-ci=?)
              (begin
                (report-error context: (format "(import-csv-file \"~a\")" path)
                              error: #f
                              message: (str (format "Error importing the CSV file '~a'; " path)
                                            (format " unable to convert CSV data.")))
                #f)
              (let* ((make-row-fields (lambda (row-strings) (map make-field row-strings)))
                     (field-lists (map make-row-fields row-data))
                     (rows (map (lambda (field-list) (make-row field-list #f)) 
                                field-lists))
                     (s (store:make version: (current-store-format) columns: '() show-deleted: #f
                                    column-layout: '() window-layout: '()
                                    sort-column: #f sort-reversed: #f rows: rows notes: ""))
                     (cols (map (lambda (l) (make-column l #f)) labels)))
                (store.set-columns! s cols)
                s)))
        #f)))

(define (write-csv-file store dest-path)
  (let* ((col-line (apply (partial string-join ",")
                          (map (lambda (c) (format "\"~a\"" (column-label c)))
                               (store-columns store))))
         (row-lines (map (lambda (r) 
                           (apply (partial string-join  ",")
                                  (map (lambda (f) (format "\"~a\"" (field-value f)))
                                       (row-fields r)))) 
                         (store-rows store)))
         (out-lines (cons col-line row-lines))
         (data (apply (partial string-join (format "~%")) out-lines))
         (doc-bytes (object->u8vector data))
         (byte-count (u8vector-length doc-bytes))
         (out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () (write-subu8vector doc-bytes 0 byte-count out))
        (lambda () (close-output-port out)))
    dest-path))
