;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store-tests.scm
;;;; Project:       Delectus
;;;; Purpose:       unit tests for store.scm
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Basic data structures
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; test data
;;; ----------------------------------------------------------------------

(define $column-labels-1 '("One" "Two" "Three" "Four" "Five" "Six"))

(define $row-values-1 '("Fred" "Barney" "Wilma" "Betty" "Pebbles" "Bam Bam"))
(define $row-values-2 '("Gold" "Silver" "Bronze" "Copper" "Platinum" "Palladium"))
(define $row-values-3 '("Lion" "Tiger" "Bear" "Wolf" "Fox" "Jackal"))

;;; ----------------------------------------------------------------------
;;; fields
;;; ----------------------------------------------------------------------

(define test:field-creation
  (let ((test-val "Test"))
    (unit-test "Field creation" 
               (lambda () 
                 (let ((f (make-field test-val)))
                   (if (field? f)
                       (let ((found-val (field.value f)))
                         (if (equal? test-val found-val)
                           (succeed #t)
                           (fail (format "field-value returned the wrong value; expected ~a, but found ~a"
                                         test-val found-val))))
                       (fail (format "(field? (make-field \"~a\")) returned false" test-val))))))))

(define test:field-serialization
  (let ((test-val "Test"))
    (unit-test "Field serialization" 
               (lambda () 
                 (let* ((in-f (make-field test-val))
                        (ser-f (field.serialize in-f))
                        (out-f (field.deserialize ser-f)))
                   (if (field? out-f)
                       (let ((in-v (field.value in-f))
                             (out-v (field.value out-f)))
                         (if (equal? in-v out-v)
                             (succeed #t)
                             (fail (format "Input value: ~a; output value: ~a" in-v out-v))))
                       (fail (format "deserialize-field returned a value that was not a field: ~a" out-f))))))))

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------


(define test:row-creation
  (let ((test-fields (map make-field $row-values-1)))
    (unit-test "Row creation" 
               (lambda () 
                 (let ((r (make-row test-fields #t)))
                   (if (row? r)
                       (if (= (length test-fields) (length (row.fields r)))
                           (if (every? id (map (lambda (x y) (equal? (field.value x)(field.value y))) 
                                               test-fields (row.fields r)))
                               (succeed #t)
                               (fail (format "The created row contained values not equal to the inputs: in: ~a; out: ~a"
                                             test-fields (row.fields r))))
                           (fail (format "count of test fields: ~a; count of test row's fields: ~a"
                                         (length test-fields)(length (row.fields r)))))
                       (fail (format "(row? (make-row \"~a\")) returned false" test-fields))))))))

(define test:row-serialization
  (let ((test-fields (map make-field $row-values-1)))
    (unit-test "Row serialization" 
               (lambda () 
                 (let* ((in-row (make-row test-fields #f))
                        (ser-row (row.serialize in-row))
                        (out-row (row.deserialize ser-row)))
                   (if (row? out-row)
                       (if (every? id (map (lambda (x y) (equal? (field.value x)(field.value y))) 
                                           (row.fields in-row)(row.fields out-row)))
                           (succeed #t)
                           (fail (format "The output row contained values not equal to the inputs: in: ~a; out: ~a"
                                         (row.fields in-row)(row.fields out-row))))
                       (fail (format "deserialize-row returned a value that was not a row: ~a" out-row))))))))

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define test:column-creation
  (unit-test "Column creation" 
             (lambda () 
               (let* ((test-label "Test")
                      (test-width 200.0)
                      (c (make-column test-label #f)))
                 (if (column? c)
                     (if (equal? test-label (column.label c))
                         (succeed #t)
                         (fail (format "The output column's label was not equal to the input: in: ~a; out: ~a"
                                       test-label (column.label c))))
                     (fail (format "(column? (make-column \"~a\"  ~a)) returned false" 
                                   test-label #f)))))))

(define test:column-serialization
  (unit-test "Column serialization" 
             (lambda () 
               (let* ((test-label "Test")
                      (test-width 200.0)
                      (in-c (make-column test-label #f))
                      (ser-c (column.serialize in-c))
                      (out-c (column.deserialize ser-c)))
                 (if (column? out-c)
                     (if (equal? (column.label in-c)(column.label out-c))
                         (succeed #t)
                         (fail (format "The output column's label was not equal to the input: in: ~a; out: ~a"
                                       (column.label in-c)(column.label out-c))))
                     (fail (format "deserialize-column returned a value that was not a column: ~a" out-c)))))))

;;; ----------------------------------------------------------------------
;;; stores
;;; ----------------------------------------------------------------------

(define (test:shuffle ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (let ((ind (random-integer (length items))))
          (loop (append (take ind items)
                        (drop (+ ind 1) items))
                (cons (list-ref items ind)
                      result))))))

(define test:store-creation
  (unit-test "Store creation" 
             (lambda () 
               (let* ((test-columns (map (lambda (l) (make-column l #f))
                                         $column-labels-1))
                      (test-show-deleted? #f)
                      (test-column-layout (map (lambda (lbl) (cons lbl 150.0))
                                               (test:shuffle $column-labels-1)))
                      (test-to-do-column #t)
                      (test-sort (list-ref $column-labels-1 (random-integer (length $column-labels-1))))
                      (test-rows (map (lambda (r) (make-row (map make-field r)
                                                            #f))
                                      (list $row-values-1 $row-values-2 $row-values-3)))
                      (test-notes "Foo bar baz")
                      (s (make-store (current-store-format)
                                     test-columns
                                     test-show-deleted?
                                     test-column-layout
                                     test-to-do-column
                                     test-sort
                                     #f
                                     test-rows
                                     test-notes)))
                 (if (store? s)
                     (if (equal? (column.label (list-ref test-columns 3))
                                 (column.label (list-ref (store.columns s) 3)))
                         (succeed #t)
                         (fail (format "found a column in the created store that doesn't match its input: in: ~a; out: ~a"
                                       test-columns (store.columns s))))
                     (fail (format "make-store returned a value that is not a store: ~a" s)))))))

(define test:store-serialization
  (unit-test "Store serialization" 
             (lambda () 
               (let* ((test-columns (map (lambda (l) (make-column l #f))
                                         $column-labels-1))
                      (test-show-deleted? #f)
                      (test-column-layout (map (lambda (lbl) (cons lbl 150.0))
                                               (test:shuffle $column-labels-1)))
                      (test-to-do-column #f)
                      (test-sort (list-ref $column-labels-1 (random-integer (length $column-labels-1))))
                      (test-rows (map (lambda (r) (make-row (map make-field r)
                                                            #f))
                                      (list $row-values-1 $row-values-2 $row-values-3)))
                      (test-notes "Foo bar baz")
                      (in-s (make-store (current-store-format)
                                        test-columns
                                        test-show-deleted?
                                        test-column-layout
                                        test-to-do-column
                                        test-sort
                                        #f
                                        test-rows
                                        test-notes))
                      (ser-s (store.serialize in-s))
                      (out-s (store.deserialize ser-s)))
                 (if (store? out-s)
                     (if (every? id (map (lambda (x y) (equal? (column.label x)
                                                               (column.label y)))
                                         (store.columns in-s)(store.columns out-s)))
                         (let ((out-rows (store.rows out-s))
                               (in-rows (store.rows in-s)))
                           (if (= (length out-rows)(length in-rows))
                               (let* ((field=? (lambda (f1 f2) (equal? (field.value f1)(field.value f2))))
                                      (row=? (lambda (r1 r2) (every? id (map field=? (row.fields r1)(row.fields r2))))))
                                 (if (every? id (map row=? out-rows in-rows))
                                     (succeed #t)
                                     (fail (str
                                            (format "The output rows should match the input rows, ")
                                            (format "but didn't.")
                                            (format "~%   input: ~a" in-rows)
                                            (format "~%  output: ~a" out-rows)))))
                               (fail (format "the row-count of the ouput store doesn't match the input's: in: ~a; out: ~a"
                                             (length (store.rows in-s))(length (store.rows out-s))))))
                         
                         (fail (format "found a column in the ouput store that doesn't match its input: in: ~a; out: ~a"
                                       (store.columns in-s)(store.columns out-s))))
                     (fail (format "deserialize-store returned a value that is not a store: ~a" out-s)))))))

;;; ----------------------------------------------------------------------
;;; suite
;;; ----------------------------------------------------------------------

(define test-suite:store-tests (test-suite "Store tests"
                                         test:field-creation
                                         test:field-serialization
                                         test:row-creation
                                         test:row-serialization
                                         test:column-creation
                                         test:column-serialization
                                         test:store-creation
                                         test:store-serialization
                                         ))


