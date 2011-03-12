;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-csv.scm
;;;; Project:       Delectus
;;;; Purpose:       a tool to generate a test table in csv form
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (genchars)
  (let* ((charcount 256)
         (charpool "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ")
         (charpoolsize (string-length charpool)))
    (let loop ((i 0)
               (result '()))
      (if (< i charcount)
          (loop (+ i 1)(cons (string-ref charpool (random-integer charpoolsize)) result))
          (reverse result)))))

(define (gen-string-length)
  (+ 1 (random-integer 9)))

(define (generate-test-string seedx seedy)
  (let* ((chars (genchars))
         (charcount (length chars))
         (len (gen-string-length)))
    (let loop ((i 0)
               (result '()))
      (if (< i len)
          (let* ((j (+ (random-integer (+ 1 seedx))(random-integer (+ 1 seedy))))
                 (k (remainder j charcount)))
            (loop (+ i 1)(cons (list-ref chars k) result)))
          (list->string (reverse result))))))

(define (generate-test-line colcount rowseed)
  (apply string-join
         (cons ","
               (map (lambda (colseed)(generate-test-string colseed rowseed))
                    (range 0 colcount)))))

(define (write-test-table path colcount rowcount)
  (let* ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file path)))
        (lambda ()
          (let loop ((i 0))
            (if (< i rowcount)
                (begin
                  (display (generate-test-line colcount i)
                         out)
                  (newline out)
                  (loop (+ i 1))))))
        (lambda () (close-output-port out)))))

;;; (write-test-table "/Users/mikel/Desktop/test-table.csv" 64 16000)