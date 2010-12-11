;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Delectus
;;;; Purpose:       run all unit tests for Delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (run-all-tests)
  (begin
    ;; db tests
    (run-test-suite test-suite:store-tests)
    ;; io tests
    (run-test-suite test-suite:store-io-tests)))