;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store-io-tests.scm
;;;; Project:       Delectus
;;;; Purpose:       unit tests for store-io.scm
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Basic data structures
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; test data
;;; ----------------------------------------------------------------------

(define $test-dir (string-append $root "/test"))
(define $csv-test-path (string-append $test-dir "/Movies.csv"))
(define $csv-broken-path (string-append $test-dir "/broken.csv"))
(define $delectus-test-path (string-append $test-dir "/Movies.delectus"))

;;; ----------------------------------------------------------------------
;;; Unit tests
;;; ----------------------------------------------------------------------

(define test:write/read
  (unit-test "Read a CSV file. then write and then read a Delectus file" 
               (lambda () 
                 (let ((s (read-csv-file $csv-test-path)))
                   (if (store? s)
                       (begin
                         (write-delectus-file s $delectus-test-path)
                         (let ((s (read-delectus-file $delectus-test-path)))
                           (if (store? s)
                               (succeed #t)
                               (fail (format "Delectus read failed on file '~a'; read-delectus-file returned: ~a"
                                             $delectus-test-path s)))))
                       (fail (format "CSV read failed on file '~a'; read-csv-file returned: ~a"
                                     $csv-test-path s)))))))

;;; ----------------------------------------------------------------------
;;; suite
;;; ----------------------------------------------------------------------

(define test-suite:store-io-tests (test-suite "Store I/O tests"
                                              test:write/read
                                              ))


