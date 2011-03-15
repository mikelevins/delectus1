;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Delectus
;;;; Purpose:       run all unit tests for Delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; entries
;;; ----------------------------------------------------------------------

(define (test:entry)
  (let* ((control1 (random-integer 1024))
         (control-str1 (number->string control1))
         (control2 (random-integer 1024))
         (control-str2 (number->string control2))
         (control-str3 "Foobar!")
         (entry (entry:make control-str1)))
    (test:expect 'test:entry:val1 (string=? control-str1 (entry:value entry)))
    (test:expect 'test:entry:num1 (= control1 (%entry-number-value entry)))
    (entry:set-value! entry control-str2)
    (test:expect 'test:entry:val2 (string=? control-str2 (entry:value entry)))
    (test:expect 'test:entry:num2 (= control2 (%entry-number-value entry)))
    (entry:set-value! entry control-str3)
    (test:expect 'test:entry:val3 (string=? control-str3 (entry:value entry)))
    (test:expect 'test:entry:num3 (not (%entry-number-value entry)))
    ))

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------

(define $row-values1 '("Fred" "Square" "Orange"))
(define $row-values2 '("0" "1" "2"))
(define $row-values3 '("Wilma" "Slim" "White"))

(define (test:row)
  (let* ((r1 (row:make $row-values1))
         (r2 (row:make $row-values2))
         (r3 (row:make $row-values3))
         (new-elt1 "4")
         (new-elt2 "16")
         (r4 (row:make-with-row-entries r3 '(0 2))))
    (test:expect 'test:row:make (row? r1))
    (test:expect 'test:row:element (string=? "Fred" (row:element r1 0)))
    (test:expect 'test:row:element-as-number (and (not (row:element-as-number r1 2))
                                                  (= 2 (row:element-as-number r2 2))))
    (test:expect 'test:row:position (= 2 (row:position r3 "White")))
    (row:set-element! r2 2 new-elt1)
    (test:expect 'test:row:set-element! (= 4 (row:element-as-number r2 2)))
    (test:expect 'test:row:add-element!:before (= 3 (row:count-elements r2)))
    (row:add-element! r2 new-elt2)
    (test:expect 'test:row:add-element!:after (and (= 4 (row:count-elements r2))
                                                   (= 16 (row:element-as-number r2 3))))
    (test:expect 'test:row:match-text:true (row:match-text? r1 "ge"))
    (test:expect 'test:row:match-text:false (not (row:match-text? r3 "zb")))
    (test:expect 'test:row:make-with-row-entries (and (= 2 (row:count-elements r4))
                                                      (string=? "Wilma" (row:element r4 0))
                                                      (string=? "White" (row:element r4 1))))))

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define (test:column)
  (let* ()
    #t))

;;; ----------------------------------------------------------------------
;;; test runner
;;; ----------------------------------------------------------------------

(define (run-all-tests)
  (newline)
  (display "Delectus unit tests")

  (newline)(newline)
  (display "  Entries:")
  (newline)
  (test:entry)

  (newline)(newline)
  (display "  Rows:")
  (newline)
  (test:row)

  (newline)(newline)
  (display "  Columns:")
  (newline)
  (test:column)

  (newline)(newline))

;;; (run-all-tests)

