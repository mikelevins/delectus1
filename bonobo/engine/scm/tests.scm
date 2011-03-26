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
  (let* ((lbl "Foo!")
         (col (column:make lbl)))
    (test:expect 'test:column:label (string=? lbl (column:label col)))
    (test:expect 'test:column:deleted? (not (column:deleted? col)))
    (column:set-deleted! col #t)
    (test:expect 'test:column:set-deleted! (column:deleted? col))))

;;; ----------------------------------------------------------------------
;;; column-sequences
;;; ----------------------------------------------------------------------

(define $col-labels1 '("Name" "Shape" "Color"))

(define (test:column-sequence)
  (let* ((colseq (column-sequence:make $col-labels1)))
    (test:expect 'test:column-sequence:make (column-sequence? colseq))
    (test:expect 'test:column-sequence:element (string=? "Color"
                                                         (column:label (column-sequence:element colseq 2))))
    (test:expect 'test:column-sequence:labels
                 (let ((lbls (vector->list (column-sequence:labels colseq))))
                   (equal? lbls $col-labels1)))
    (test:expect 'test:column-sequence:position (= 1 (column-sequence:position colseq "Shape")))))

;;; ----------------------------------------------------------------------
;;; tables
;;; ----------------------------------------------------------------------

(define $tcol-labels '("First Name" "Last Name" "Color" "Shape"))
(define $trow-vals0 '("Fred" "Flintstone" "Orange" "Square"))
(define $trow-vals1 '("Wilma" "Flintstone" "White" "Slim"))
(define $trow-vals2 '("Barney" "Rubble" "Brown" "Round"))
(define $trow-vals3 '("Betty" "Rubble" "Blue" "Slim"))

(define (test:table)
  (let ((tbl1 (table:make columns: $tcol-labels rows: (list $trow-vals0 $trow-vals1 $trow-vals2 $trow-vals3))))
    (test:expect 'test:table:make (delectus-table? tbl1))
    (test:expect 'test:table:column-labels (equal? $tcol-labels (table:column-labels tbl1)))
    (test:expect 'test:table:count-columns (= 4 (table:count-columns tbl1)))
    (test:expect 'test:table:count-rows (= 4 (table:count-rows tbl1)))
    (test:expect 'test:table:column-index (= 3 (table:column-index tbl1 "Shape")))
    (test:expect 'test:table:column-at (string=? "Color" (column:label (table:column-at tbl1 "Color"))))
    (test:expect 'test:table:row-at (string=? "Blue" (row:element (table:row-at tbl1 3)
                                                                  (table:column-index tbl1 "Color"))))
    (test:expect 'test:table:column-values (equal? '("Flintstone" "Flintstone" "Rubble" "Rubble")
                                                   (table:column-values tbl1 "Last Name")))
    (test:expect 'test:table:numeric-column (not (table:numeric-column? tbl1 "First Name")))
    (test:expect 'test:table:add-row:before (= 4 (table:count-rows tbl1)))
    (table:add-row! tbl1)
    (test:expect 'test:table:add-row:after (= 5 (table:count-rows tbl1)))
    (test:expect 'test:table:add-column:before (= 4 (table:count-columns tbl1)))
    (table:add-column! tbl1 "Number")
    (test:expect 'test:table:add-column:after (= 5 (table:count-columns tbl1)))
    (row:set-element! (table:row-at tbl1 0)
                      (table:column-index tbl1 "Number") "0")
    (row:set-element! (table:row-at tbl1 1)
                      (table:column-index tbl1 "Number") "1")
    (row:set-element! (table:row-at tbl1 2)
                      (table:column-index tbl1 "Number") "2")
    (row:set-element! (table:row-at tbl1 3)
                      (table:column-index tbl1 "Number") "3")
    (row:set-element! (table:row-at tbl1 4)
                      (table:column-index tbl1 "Number") "4")
    (test:expect 'test:table:numeric-column? (table:numeric-column? tbl1 "Number"))
    (test:expect 'test:table:mark-column-deleted:before (not (column:deleted? (table:column-at tbl1 "Number"))))
    (table:mark-column-deleted! tbl1 "Number" #t)
    (test:expect 'test:table:mark-column-deleted:true (column:deleted? (table:column-at tbl1 "Number")))
    (table:mark-column-deleted! tbl1 "Number" #f)
    (test:expect 'test:table:mark-column-deleted:false (not (column:deleted? (table:column-at tbl1 "Number"))))

    (test:expect 'test:table:mark-row-deleted:before (not (row:deleted? (table:row-at tbl1 2))))
    (table:mark-row-deleted! tbl1 2 #t)
    (test:expect 'test:table:mark-row-deleted:true (row:deleted? (table:row-at tbl1 2)))
    (table:mark-row-deleted! tbl1 2 #f)
    (test:expect 'test:table:mark-row-deleted:false (not (row:deleted? (table:row-at tbl1 2))))))

;;; ----------------------------------------------------------------------
;;; documents
;;; ----------------------------------------------------------------------

(define $dcol-labels '("First Name" "Last Name" "Color" "Shape" "Age"))
(define $drow-vals0 '("Fred" "Flintstone" "Orange" "Square" "30"))
(define $drow-vals1 '("Wilma" "Flintstone" "White" "Slim" "32"))
(define $drow-vals2 '("Barney" "Rubble" "Brown" "Round" "31"))
(define $drow-vals3 '("Betty" "Rubble" "Blue" "Slim" "33"))

(define (test:document)
  (let* ((docid1 (new-document))
         (doc2 (doc:make table: (table:make columns: $dcol-labels
                                            rows: (list $drow-vals0 $drow-vals1 $drow-vals2 $drow-vals3))))
         (docid2 (doc:register! (next-document-id) doc2)))
    (test:expect 'test:document:new (document? doc2))
    (test:expect 'test:document:find-document (document? (find-document docid1)))
    (test:expect 'test:document:count-columns (= 5 (count-columns docid2)))
    (test:expect 'test:document:column-at-index (string=? "Color" (column-at-index docid2 2)))
    (test:expect 'test:document:count-rows (= 4 (count-rows docid2)))
    (test:expect 'test:document:value-at (string=? "Rubble" (value-at docid2 "Last Name" 2)))
    (put-value-at! docid2 "Last Name" 2 "Stone")
    (test:expect 'test:document:put-value-at! (string=? "Stone" (value-at docid2 "Last Name" 2)))
    (test:expect 'test:document:row-finished? (not (row-finished? docid2 2)))
    (mark-row-finished! docid2 2 #t)
    (test:expect 'test:document:mark-row-finished! (row-finished? docid2 2))
    (add-row! docid2)
    (test:expect 'test:document:add-row! (= 5 (count-rows docid2)))
    (add-column! docid2 "Test")
    (test:expect 'test:document:add-column! (= 6 (count-columns docid2)))
    (test:expect 'test:document:column-deleted? (not (column-deleted? docid2 "Test")))
    (mark-column-deleted! docid2 "Test" #t)
    (test:expect 'test:document:mark-column-deleted! (column-deleted? docid2 "Test"))
    (test:expect 'test:document:row-deleted? (not (row-deleted? docid2 4)))
    (let ((r (table:row-at (doc:table doc2) 4)))
      (mark-row-deleted! docid2 4 #t)
      (test:expect 'test:document:mark-row-deleted! (row:deleted? r)))
    (test:expect 'test:document:column-has-total?:false (not (column-has-total? docid2 "Color")))
    (test:expect 'test:document:column-has-total?:true (column-has-total? docid2 "Age"))
    (test:expect 'test:document:column-total (= 126 (column-total docid2 "Age")))
    (compact! docid2)
    (test:expect 'test:document:compact!:rows (= 4 (count-rows docid2)))
    (test:expect 'test:document:compact!:columns (= 5 (count-columns docid2)))))

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

  (newline)(newline)
  (display "  Column sequences:")
  (newline)
  (test:column-sequence)

  (newline)(newline)
  (display "  Tables:")
  (newline)
  (test:table)

  (newline)(newline)
  (display "  Documents:")
  (newline)
  (test:document)

  (newline)(newline))

;;; (run-all-tests)

;;; (define $test-path "/Users/mikel/Desktop/zipcode.delectus")
;;; (define $d1 (read-delectus-file $test-path))
;;; (value-at $d1 "city" 0)
;;; (put-value-at! $d1 "costar" 0 "Foo")
;;; $d1
;;; (define $doc (find-document $d1))
;;; (document? $doc)
;;; (table:column-labels (doc:table $doc))
;;; (table:value-at (doc:table $doc) "city" 0)
;;; (doc:view-valid? $doc)

;;; (define $jrpath "/Users/mikel/Projects/delectus/delectus/test-data/junior-movies.delectus")
;;; (define $jr (read-delectus-file $jrpath))
;;; (count-rows $jr)
;;; (get-view $jr description: (view:description sort-column: "Title" sort-order: $SORT_DESCENDING filter-text: " "))
;;; (value-at $jr "Title" 0)
;;; (add-row! $jr)

;;; (api:count-deleted-columns $jr)
;;; (api:mark-column-deleted! $jr "Title" #t)