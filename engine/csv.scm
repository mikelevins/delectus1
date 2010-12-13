;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.scm
;;;; Project:       Delectus
;;;; Purpose:       parse csv files
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (parse-csv-line line sep quotech)
  (let ((line-len (string-length line))
        (result '())
        (quote-pending? #f)
        (acc ""))
    (let loop ((i 0))
      (if (>= i line-len)
          (let ((result (if (> (string-length acc) 0)
                            (cons acc result)
                            result)))
            (reverse result))
          (let* ((next-char (string-ref line i)))
            (cond
             ((char=? next-char quotech)
              (begin
                (set! quote-pending? (not quote-pending?))
                (set! acc (string-append acc (string quotech)))
                (loop (+ 1 i))))
             ((char=? next-char sep)
              (if quote-pending?
                  (begin
                    (set! acc (string-append acc (string next-char)))
                    (loop (+ 1 i)))
                  (begin
                    (set! result (cons acc result))
                    (set! acc "")
                    (loop (+ 1 i)))))
             (else
              (begin
                (set! acc (string-append acc (string next-char)))
                (loop (+ 1 i))))))))))

(define (read-csv-file path)
  (call-with-input-file path
    (lambda (in)
      (map (lambda (ln)(parse-csv-line ln #\, #\"))
       (read-all in read-line)))))

;;; (time (length (read-csv-file "/Users/mikel/Projects/delectus/test-data/zipcode.csv")))
;;; (parse-csv-line "Fred, Barney, \"Wilma, formerly Gravel\"" #\, #\")

