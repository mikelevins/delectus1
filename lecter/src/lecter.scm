;;;; ***********************************************************************
;;;;
;;;; Name:          lecter.scm
;;;; Project:       Delectus 1->2 conversion utility
;;;; Purpose:       main command-line program
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $lecter-version-string "lecter v2.0.0d1")

;;; USAGE:
;;; lecter --version => returns the version of lecter to stdio
;;; lecter --format <pathname> => name of delectus format version, or "INVALID"
;;; lecter --sexp <pathname> => returns s-expression conversion of delectus data to stdio

(define (print-lecter-usage)
  (newline)
  (display "USAGE:")(newline)
  (display "  lecter --version # prints the version of lecter")(newline)
  (display "  lecter --format PATH # prints the version number of the delectus file format,")(newline)
  (display "                       # or INVALID if it's not a recognized Delectus format")(newline)
  (display "  lecter --format-name PATH # prints the version name of the delectus file format,")(newline)
  (display "                            # or INVALID if it's not a recognized Delectus format")(newline)
  (display "  lecter --sexp # prints the Delectus data to stdio as s-expressions")(newline))

(define (->sexp path)
  (let* ((data (delectus->lisp path))
         (columns-tail (member 'COLUMNS data))
         (columns (if columns-tail (cadr columns-tail) '()))
         (rows-tail (member 'ROWS data))
         (rows (if rows-tail (cadr rows-tail) '())))
    (display ":COLUMNS (")
    (for-each (lambda (column)
                (newline)
                (let ((label (list-ref column 1))
                      (deleted? (list-ref column 3)))
                  (display ":LABEL ")(write label)
                  (display " :DELETED ")
                  (display deleted?)
                  (display " ")))
              columns)
    (display ")")
    (newline)
    (display ":ROWS")
    (for-each (lambda (row)
                (newline)
                (let ((deleted? (list-ref row 1))
                      (entries (list-ref row 3)))
                  (display "(:DELETED ")
                  (display deleted?)
                  (display " ")
                  (for-each (lambda (entry)
                              (display ":VALUE ")
                              (let ((val (list-ref entry 1)))
                                (write val)(display " ")))
                            entries)
                  (display ")")))
              rows)))

(define (->csv path)
  (delectus->csv path))

(let ((args (cdr (command-line))))
  (if (< (length args) 1)
      (print-lecter-usage)
      (let ((option (list-ref args 0)))
        (cond ((equal? option "--version") (begin (newline)
                                                  (display $lecter-version-string)
                                                  (newline)))
              ((equal? option "--format") (let* ((path (list-ref args 1))
                                                 (format-number (delectus-format-version path)))
                                            (begin (write format-number)
                                                   (newline))))
              ((equal? option "--format-name") (let* ((path (list-ref args 1))
                                                      (format-number (delectus-format-version path)))
                                                 (if (equal? "INVALID" format-number)
                                                     (begin (write format-number)
                                                            (newline))
                                                     (let ((format-name (delectus-format-number->name format-number)))
                                                       (write format-name)
                                                       (newline)))))
              ((equal? option "--sexp") (->sexp (list-ref args 1)))
              ((equal? option "--csv") (->csv (list-ref args 1)))
              (else (print-lecter-usage))))))



