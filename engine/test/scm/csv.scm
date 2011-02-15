;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.scm
;;;; Project:       Delectus
;;;; Purpose:       test code for engine/src/csv.scm
;;;; Author:        mikel evins
;;;; Requirements:  delectus loaded
;;;;
;;;; ***********************************************************************

(define (test:read-csv)
  (let* ((path (string-append $test-data "zipcode.csv"))
         (tbl (csv:read path)))
    (test:expect 'test:read-csv (= 7 (table:count-columns tbl))
                 failure: (test:report 'test:read-csv "Incorrect column count"))
    (test:expect 'test:read-csv (= 43191 (table:count-rows tbl))
                 failure: (test:report 'test:read-csv "Incorrect row count"))
    (test:expect 'test:read-csv (string-ci=? "Portsmouth"
                                             (table:value-at tbl "city" 0))
                 failure: (test:report 'test:read-csv "Incorrect first city name"))
    (test:expect 'test:read-csv (string-ci=? "Ketchikan"
                                             (table:value-at tbl "city" 43190))
                 failure: (test:report 'test:read-csv "Incorrect last city name"))))

;;;(test:read-csv)
