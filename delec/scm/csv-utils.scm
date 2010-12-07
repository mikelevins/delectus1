;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv-utils.scm
;;;; Project:       Delectus
;;;; Purpose:       csv import for Delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $default-column-labels
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

(define $default-column-labels-count (length $default-column-labels))

(define (csv:make-column-labels count #!optional (append-count 0) (acc '()))
  (let* ((csv:build-labels (lambda (c lbs) (map (partial string-repeat (+ c 1)) lbs))))
    (if (<= count 0)
        acc
        (if (<= count $default-column-labels-count)
            (append acc (csv:build-labels append-count (take count $default-column-labels)))
            (csv:make-column-labels (- count $default-column-labels-count)
                                (+ append-count 1)
                                (append acc (csv:build-labels append-count 
                                                              (take $default-column-labels-count $default-column-labels))))))))

(define (csv:get-csv-column-labels data headers-in-first-line?)
  (or (and headers-in-first-line? (car data))
      (csv:make-column-labels (length (car data)))))

(define (csv:get-csv-column-entries data headers-in-first-line?)
  (or (and headers-in-first-line? (cdr data))
      data))

(define (csv:empty-csv-row? r)
  (or (null? r)
      (every (disjoin null? (partial string=? "")) r)))

(define (csv:remove-empty-csv-elements data)
  (filter (complement csv:empty-csv-row?) data))

(define (csv:well-formed-csv? data)
  (or (null? data)
      (let* ((field-count (length (car data)))
             (not-empty? (complement csv:empty-csv-row?))
             (count-valid? (lambda (e) (= field-count (length e)))))
        (every (conjoin not-empty?
                        count-valid?)
               data))))

(define (csv:canonicalize-csv-list data)
  (let ((data (csv:remove-empty-csv-elements data)))
    (if (csv:well-formed-csv? data)
        data
        (error "ill-formed CSV data"))))

(define (csv:read-csv-file path)
  (let ((reader (lambda ()
                  (csv:canonicalize-csv-list
                   (csv->list
                    (make-csv-reader
                     (open-input-file path)
                     '((separator-chars            . (#\,))
                       (strip-leading-whitespace?  . #t)
                       (strip-trailing-whitespace? . #t)))))))
        (error-handler (lambda (err)
                         (report-error context: (format "(csv:read-csv-file ~a)" path)
                                       error: err
                                       message: (format "Error reading the file '~a'; Could not read CSV data" path))
                         #f)))
    (with-exception-catcher error-handler reader)))

