;;;; ***********************************************************************
;;;;
;;;; Name:          lecter.scm
;;;; Project:       Delectus 1->2 conversion utility
;;;; Purpose:       main command-line program
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; version
;;; ======================================================================

(define $lecter-version-string "1.5.0d1")

;;; ======================================================================
;;; UI
;;; ======================================================================

;;; USAGE:
;;; lecter --version => returns the version of lecter to stdio
;;; lecter --format <pathname> => name of delectus format version, or "INVALID"
;;; lecter --sexp <pathname> => returns s-expression conversion of delectus data to stdio
;;; lecter --csv <pathname> => returns csv conversion of delectus data to stdio
;;; lecter --jsonl <pathname> => returns csv conversion of delectus data to stdio

(define (print-lecter-usage)
  (newline)
  (display "USAGE:")(newline)
  (display "  lecter --version # prints the version of lecter")(newline)
  (display "  lecter --uuid # prints a newly-generated v4 UUID")(newline)
  (display "  lecter --format PATH # prints the version name of the delectus file format,")(newline)
  (display "                            # or INVALID if it's not a recognized Delectus format")(newline)
  (display "  lecter --sexp PATH # prints the Delectus data to stdio as s-expressions")(newline)
  (display "  lecter --raw PATH # prints the serialized data structures stored in the Delectus file")(newline)
  (display "  lecter --gambit PATH # prints the deserialized Gambit objects stored in the Delectus file")(newline)
  (display "  lecter --csv PATH # prints the Delectus data to stdio as CSV")(newline)
  (display "  lecter --json PATH # writes the Delectus data to stdio as JSON")(newline)
  (display "  lecter --jsonl PATH # prints the Delectus data to stdio as json, one object per line")(newline))


(let ((args (cdr (command-line))))
  (if (< (length args) 1)
      (print-lecter-usage)
      (let ((option (list-ref args 0)))
        (cond ((equal? option "--version") (begin (newline)
                                                  (display $lecter-version-string)
                                                  (newline)))
              ((equal? option "--uuid") (begin (write (make-uuid))
                                               (newline)))
              ((equal? option "--format") (let* ((path (list-ref args 1))
                                                 (format-number (delectus-format-version path)))
                                            (if (equal? "INVALID" format-number)
                                                (begin (display "Not a valid Delectus file")
                                                       (newline))
                                                (let ((format-name (delectus-format-number->name format-number)))
                                                  (display format-name)
                                                  (newline)))))
              ((equal? option "--sexp") (write-sexp (list-ref args 1)))
              ((equal? option "--raw") (write-raw (list-ref args 1)))
              ((equal? option "--gambit") (write-gambit (list-ref args 1)))
              ((equal? option "--csv") (write-csv (list-ref args 1)))
              ((equal? option "--json") (write-json (list-ref args 1)))
              ((equal? option "--jsonl") (write-couchdb (list-ref args 1)))
              (else (print-lecter-usage))))))



