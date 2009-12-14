;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-tests.scm
;;;; Project:       Delectus
;;;; Purpose:       unit tests for document.scm
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Basic data structures
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; document creation
;;; ----------------------------------------------------------------------

(define test:document-creation-1
  (unit-test "Document creation 1" 
             (lambda () 
               (let* ((docID (next-document-id))
                      (store (make-store (current-store-format) '() #f '() '() #f #f '() ""))
                      (doc (document store)))
                 (register-document! docID doc)
                 (let ((maybe-doc (get-document docID)))
                   (if maybe-doc
                       (if (eqv? store (document-store doc))
                           (succeed #t)
                           (fail (str
                                  (format "Creating a new document failed. ")
                                  (format "Looking up the ID of a newly-created document ")
                                  (format "found a different document"))))
                       (fail (str
                              (format "Creating a new document failed. ")
                              (format "Looking up the ID of a newly-created document ")
                              (format "failed to find that document")))))))))

(define test:document-creation-2
  (unit-test "Document creation 2" 
             (lambda () 
               (let* ((docID (get-new-document!))
                      (maybe-doc (get-document docID)))
                 (if docID
                     (if maybe-doc
                         (suceed #t)
                         (fail (str
                                (format "Creating a new document failed. ")
                                (format "Looking up the ID of a newly-created document ")
                                (format "failed to find that document"))))
                     (fail (format "get-new-document returned #f"))))))) 

;;; ----------------------------------------------------------------------
;;; data source methods
;;; ----------------------------------------------------------------------

(define test:count-rows
  (unit-test "Count document rows"
             (lambda ()
               (let* ((docID (get-new-document!))
                      (doc (get-document docID)))
                 (if (zero? (count-filtered-rows doc))
                     (let ((row-count 11))
                       (let loop ((i row-count))
                         (if (< i 1)
                             (if (= row-count
                                    (count-filtered-rows doc))
                                 (succeed #t)
                                 (fail (str
                                        (format "Added ~a rows to the test document, " row-count)
                                        (format "but the resulting row-count was ~a" (count-filtered-rows doc)))))
                             (begin
                               (add-row! doc)
                               (loop (- i 1))))))
                     (fail (str
                            (format "Created a new empty document, ")
                            (format "but its reported row count was ~a" (count-filtered-rows doc)))))))))

(define test:filter-rows
  (unit-test "Filter document rows"
             (lambda ()
               (let* ((docID (next-document-id))
                      (val-ab "AB")
                      (val-bc "bc")
                      (val-de "dE")
                      (val-fg "fg")
                      (val-hi "HI")
                      (val-empty #f)
                      (val-fa "fA")
                      (val-hb "HB")
                      (val-jd "Jd")
                      (field-ab (make-field val-ab))
                      (field-bc (make-field val-bc))
                      (field-de (make-field val-de))
                      (field-fg (make-field val-fg))
                      (field-hi (make-field val-hi))
                      (field-empty (make-field val-empty))
                      (field-fa (make-field val-fa))
                      (field-hb (make-field val-hb))
                      (field-jd (make-field val-jd))
                      (row-1 (make-row (list field-ab field-bc field-de) #f))
                      (row-2 (make-row (list field-fg field-hi field-empty) #f))
                      (row-3 (make-row (list field-fa field-hb field-jd) #f))
                      (lbl-1 "One")
                      (lbl-2 "Two")
                      (lbl-3 "Three")
                      (col-1 (make-column lbl-1 #f))
                      (col-2 (make-column lbl-2 #f))
                      (col-3 (make-column lbl-3 #f))
                      (store (make-store (current-store-format)
                                         (list col-1 col-2 col-3) #f '() '() #f #f
                                         (list row-1 row-2 row-3) ""))
                      (doc (document store)))
                 ;;; 1. count rows unfiltered
                 (if (= (count-filtered-rows doc)
                        (length (document.rows doc)))
                     ;;; add a filter and try again
                     (let ((filter-j "Jd")
                           (expected-count 1))
                       (set-filter! doc filter-j)
                       (if (= expected-count (count-filtered-rows doc))
                           ;;; change the filter to one that matches mixed-case and try again
                           (let ((filter-b "b")
                                 (expected-count 2))
                             ;;; change once more to a filter that should match nothing
                             (set-filter! doc filter-b)
                             (if (= expected-count (count-filtered-rows doc))
                                 (let ((filter-x "XXX")
                                       (expected-count 0))
                                   (set-filter! doc filter-x)
                                   (if (= expected-count (count-filtered-rows doc))
                                       ;;; hide a row and test that matching it fails
                                       (let ((filter-h "h")
                                             (expected-count 1))
                                         (set-filter! doc filter-h)
                                         (row.set-deleted! row-3 #t)
                                         (document.hide-deleted! doc)
                                         (if (= expected-count (count-filtered-rows doc))
                                             (succeed #t)
                                             (fail (str
                                                    (format "filter-h: count-filtered-rows reports ~a, " 
                                                            (count-filtered-rows doc))
                                                    (format "but the filter should yield ~a" expected-count)))))
                                       (fail (str
                                              (format "filter-x: count-filtered-rows reports ~a, " (count-filtered-rows doc))
                                              (format "but the filter should yield ~a" expected-count)))))
                                 (fail (str
                                        (format "filter-b: count-filtered-rows reports ~a, " (count-filtered-rows doc))
                                        (format "but the filter should yield ~a" expected-count)))))
                           (fail (str
                                  (format "filter-j: count-filtered-rows reports ~a, " (count-filtered-rows doc))
                                  (format "but the filter should yield ~a" expected-count)))))
                     (fail (str
                            (format "The test document has ~a rows,  " (length (document.rows doc)))
                            (format "but count-filtered-rows reports ~a, " (count-filtered-rows doc))
                            (format "even though the document is unfiltered."))))))))

(define test:get-value
  (unit-test "Get Value"
             (lambda ()
               (let* ((docID (next-document-id))
                      (val "test-val")
                      (field (make-field val))
                      (row (make-row (list field) #f))
                      (lbl "Test")
                      (col (make-column lbl #f))
                      (store (make-store (current-store-format)(list col) #f '() '() #f #f (list row) ""))
                      (doc (document store))
                      (found-val (value-for-cell doc lbl 0)))
                 (if found-val
                     (if (string=? val found-val)
                         (succeed #t)
                         (fail (str 
                                (format "Getting a cell value failed. ")
                                (format "The value returned by value-for-cell didn't match ")
                                (format "the test value."))))
                     (fail (str 
                            (format "Getting a cell value failed. ")
                            (format "value-for-cell returned #f. "))))))))

(define test:set-value
  (unit-test "Set Value"
             (lambda ()
               (let* ((docID (next-document-id))
                      (in-val "test-val")
                      (out-val "test-val 2")
                      (field (make-field in-val))
                      (row (make-row (list field) #f))
                      (lbl "Test")
                      (col (make-column lbl #f))
                      (store (make-store (current-store-format)(list col) #f '() '() #f #f (list row) ""))
                      (doc (document store))
                      (found-val (value-for-cell doc lbl 0)))
                 (if found-val
                     (if (string=? in-val found-val)
                         (begin
                           (set-value-for-cell! doc lbl 0 out-val)
                           (if (string=? out-val (value-for-cell doc lbl 0))
                               (succeed #t)
                               (fail (str 
                                      (format "Getting a cell value failed. ")
                                      (format "The value returned by value-for-cell didn't match ")
                                      (format "the output value.")))))
                         (fail (str 
                                (format "Getting a cell value failed. ")
                                (format "The value returned by value-for-cell didn't match ")
                                (format "the input value."))))
                     (fail (str 
                            (format "Getting a cell value failed. ")
                            (format "value-for-cell returned #f. "))))))))

;;; ----------------------------------------------------------------------
;;; the test suite
;;; ----------------------------------------------------------------------

(define test-suite:document-tests (test-suite "Document tests"
                                           test:document-creation-1
                                           test:count-rows
                                           test:filter-rows
                                           test:get-value
                                           test:set-value
                                           ))

