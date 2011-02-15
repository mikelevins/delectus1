;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tables.scm
;;;; Project:       Delectus
;;;; Purpose:       test code for engine/src/tables.scm
;;;; Author:        mikel evins
;;;; Requirements:  delectus loaded
;;;;
;;;; ***********************************************************************

(define (test:empty-table)
  (table:make))

(define (test:table-3cols-4rows)
  (table:make columns: '("Name" "Home Town" "Age")
              rows: '(("Fred" "Bedrock" "34")
                      ("Wilma" "Bedrock" "35")
                      ("Barney" "Bedrock" "36")
                      ("Betty" "Bedrock" "37"))))

(define (test:table:make)
  (test:expect 'test:table:make (delectus-table? (test:empty-table))))

(define (test:table:column-at)
  (let* ((tbl (test:table-3cols-4rows))
         (col (table:column-at tbl 0)))
    (test:expect 'test:table:column-at (string-ci=? "Name" (col:label col)))))

(define (test:table:column-index)
  (let* ((tbl (test:table-3cols-4rows))
         (ind (table:column-index tbl "Age")))
    (test:expect 'test:table:column-index (= ind 2))))

(define (test:table:count-columns)
  (let* ((tbl (test:table-3cols-4rows))
         (count (table:count-columns tbl)))
    (test:expect 'test:table:count-columns (= count 3))))

(define (test:table:row-at)
  (let* ((tbl (test:table-3cols-4rows))
         (row (table:row-at tbl 3)))
    (test:expect 'test:table:row-at (row? row))))

(define (test:table:count-rows)
  (let* ((tbl (test:table-3cols-4rows))
         (count (table:count-rows tbl)))
    (test:expect 'test:table:count-rows (= count 4))))

(define (test:table:add-row)
  (let* ((tbl (test:table-3cols-4rows))
         (before-count (table:count-rows tbl)))
    (table:add-row! tbl)
    (let ((after-count (table:count-rows tbl)))
      (test:expect 'test:table:add-row
                   (and (= after-count 5)
                        (= before-count (- after-count 1)))))))

(define (test:table:add-column)
  (let* ((tbl (test:table-3cols-4rows))
         (before-count (table:count-columns tbl)))
    (table:add-column! tbl "Shape")
    (let ((after-count (table:count-columns tbl)))
      (test:expect 'test:table:add-column (= after-count 4)
                   failure: (test:report 'test:table:add-column "FAILED: wrong column count"))
      (test:expect 'test:table:add-column (string-ci=? "Shape" (col:label (table:column-at tbl 3)))
                   failure: (test:report 'test:table:add-column "FAILED: wrong column label")))))

(define (test:table:put-value-at)
  (let* ((tbl (test:table-3cols-4rows))
         (testval "Hollyrock")
         (before-val "Bedrock"))
    (test:expect 'test:table:put-value-at (string-ci=? before-val (table:value-at tbl "Home Town" 0))
                 failure: (test:report 'test:table:put-value-at "FAILED: wrong before-val"))
    (table:put-value-at! tbl "Home Town" 0 testval)
    (test:expect 'test:table:put-value-at (string-ci=? testval (table:value-at tbl "Home Town" 0))
                 failure: (test:report 'test:table:put-value-at "FAILED: wrong testval"))))

(define (test:table:columns-matching)
  (let* ((tbl (test:table-3cols-4rows))
         (coltest (lambda (col)(even? (col:index col)))))
    (test:expect 'test:table:columns-matching (= 2 (vector-length (table:columns-matching tbl coltest))))))

(define (test:table:rows-matching)
  (let* ((tbl (test:table-3cols-4rows))
         (rowtest (lambda (row)(odd? (row:index row)))))
    (test:expect 'test:table:rows-matching (= 2 (vector-length (table:rows-matching tbl rowtest))))))

(define (test:table:row-sort)
  (let* ((tbl (test:table-3cols-4rows))
         (colindex (table:column-index tbl "Name"))
         (comparator (table:make-row-comparator tbl colindex string-ci<?))
         (sorted-rows (sort (table:rows tbl) comparator))
         (first-row (vector-ref sorted-rows 0)))
    (test:expect 'test:table:row-sort (string-ci=? "Barney" (row:element first-row 0)))))

(define (test:table:mark-deleted)
  (let* ((tbl (test:table-3cols-4rows))
         (before-col-count (vector-length (table:columns-matching tbl (complement col:deleted?))))
         (before-row-count (vector-length (table:rows-matching tbl (complement row:deleted?)))))
    (table:mark-column-deleted! tbl "Age" #t)
    (table:mark-row-deleted! tbl 1 #t)
    (let ((after-col-count (vector-length (table:columns-matching tbl (complement col:deleted?))))
          (after-row-count (vector-length (table:rows-matching tbl (complement row:deleted?)))))
      (test:expect 'test:table:mark-deleted (= before-col-count 3)
                   failure: "FAILED: Wrong initial column count")
      (test:expect 'test:table:mark-deleted (= before-row-count 4)
                   failure: "FAILED: Wrong initial row count")
      (test:expect 'test:table:mark-deleted (= after-col-count 2)
                   failure: "FAILED: Wrong final column count")
      (test:expect 'test:table:mark-deleted (= after-row-count 3)
                   failure: "FAILED: Wrong final row count"))))


(define (test:table:compact)
  (let* ((tbl (test:table-3cols-4rows))
         (before-col-count (table:count-columns tbl))
         (before-row-count (table:count-rows tbl)))
    (table:mark-column-deleted! tbl "Age" #t)
    (table:mark-row-deleted! tbl 1 #t)
    (table:compact! tbl)
    (let ((after-col-count (table:count-columns tbl))
          (after-row-count (table:count-rows tbl)))
      (test:expect 'test:table:compact (= before-col-count 3)
                   failure: "FAILED: Wrong initial column count")
      (test:expect 'test:table:compact (= before-row-count 4)
                   failure: "FAILED: Wrong initial row count")
      (test:expect 'test:table:compact (= after-col-count 2)
                   failure: "FAILED: Wrong final column count")
      (test:expect 'test:table:compact (= after-row-count 3)
                   failure: "FAILED: Wrong final row count"))))

