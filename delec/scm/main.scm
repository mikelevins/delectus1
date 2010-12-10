;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.scm
;;;; Project:       Delectus
;;;; Purpose:       application startup
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;(begin (cocoa:application-main))

(define $zips #f)

(begin
  (display "Delectus: timing load and save:")
  (newline)
  (display "  input file: /Users/mikel/Projects/delectus/drafts/delectus2/test-data/zipcode.csv")
  (newline)
  (time
   (set! $zips (csv:read-csv-file "/Users/mikel/Projects/delectus/drafts/delectus2/test-data/zipcode.csv")))
  (display "  output file: /Users/mikel/Projects/delectus/delec/test/zips.delectus")
  (newline)
  (time
   (write-delectus-file $zips "/Users/mikel/Projects/delectus/delec/test/zips.delectus"))
  (display "  input file: /Users/mikel/Projects/delectus/delec/test/zips.delectus")
  (newline)
  (time
   (set! $zips1 (read-delectus-file "/Users/mikel/Projects/delectus/delec/test/zips.delectus"))))
