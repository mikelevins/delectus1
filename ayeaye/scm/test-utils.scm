;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-utils.scm
;;;; Project:       Delectus
;;;; Purpose:       utilities for running tests
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------
;;; fail: 
;;; the function fail is defined in the scope of a running unit test
;;; use it in the body of a test to report a failure like so:
;;;  (fail "Here's a message explaining why the test failed")

;;; ----------------------------------------------------------------------
;;; test data structures
;;; ----------------------------------------------------------------------

(define-structure test-suite name tests)
(define-structure unit-test name body succeeded? failure-reason)

(define $test-errors '())
(define (clear-test-errors) (set! $test-errors '()))
(define (add-test-error! test-name err) (set! $test-errors (append $test-errors (list (cons test-name err)))))

(define $test-failures '())
(define (clear-test-failures) (set! $test-failures '()))
(define (add-test-failure! test-name msg) (set! $test-failures (append $test-failures (list (cons test-name msg)))))

(define $test-successes '())
(define (clear-test-successes) (set! $test-successes '()))
(define (add-test-success! test-name) (set! $test-successes (append $test-successes (list test-name))))

(define (init-tests)
  (clear-test-errors)
  (clear-test-failures)
  (clear-test-successes))

;;; ----------------------------------------------------------------------
;;; running tests
;;; ----------------------------------------------------------------------

(define succeed #t)
(define fail #f)

(define run-unit-test 
  (lambda (t)
    (dynamic-wind
        (lambda () 
          (set! succeed
                (lambda (val)
                  (unit-test-failure-reason-set! t #f)
                  (unit-test-succeeded?-set! t #t)
                  val))
          (set! fail
                (lambda (msg)
                  (unit-test-failure-reason-set! t msg)
                  (unit-test-succeeded?-set! t #f)
                  #f)))
        (lambda ()
          (let* ((test-name (unit-test-name t))
                 (test-body (unit-test-body t))
                 (error-handler (lambda (err) (add-test-error! test-name err)))
                 (test-runner (lambda () 
                                (begin
                                  (test-body)
                                  (if (unit-test-succeeded? t) 
                                      (add-test-success! test-name)
                                      (add-test-failure! test-name 
                                                         (str (format "Test '~a' failed. " test-name)
                                                              (format "The reason was: \"~a\"" 
                                                                      (unit-test-failure-reason t)))))))))
            (with-exception-catcher error-handler test-runner)))
        (lambda () 
          (set! succeed #t)
          (set! fail #f)))))

(define (run-test-suite s)
  (init-tests)
  (display (format "~%Running test suite '~a' [~a tests]..." (test-suite-name s)(length (test-suite-tests s))))
  (let loop ((tests (test-suite-tests s)))
    (if (null? tests)
        (begin
          (display (format "done." (test-suite-name s)))
          (display (format "~%  ~a tests succeeded" (length $test-successes)))
          (display (format "~%  ~a failures reported" (length $test-failures)))
          (let loop ((fails $test-failures))
            (if (null? fails)
                #f
                (let ((f (car fails)))
                  (display (format "~%  Test: '~a' (~a)" (car f)(cdr f)))
                  (loop (cdr fails)))))
          (display (format "~%  ~a errors reported" (length $test-errors)))
          (let loop ((errs $test-errors))
            (if (null? errs)
                #f
                (let ((e (car errs)))
                  (display (format "~%  Test: '~a' (~a)" (car e)(cdr e)))
                  (loop (cdr errs)))))
          (newline))
        (begin
          (run-unit-test (car tests))
          (loop (cdr tests))))))

(define (unit-test name body) (make-unit-test name body #t #f))
(define (test-suite name . tests) (make-test-suite name tests))