;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (import-csv-file pathname headers-in-first-line?)
  (let ((store (read-csv-file pathname headers-in-first-line?)))
    (if store
        (let ((doc (document store)))
          (register-document! (next-document-id) doc))
        #f)))

(define (load-delectus-file pathname)
  (let ((store (read-delectus-file pathname)))
    (if store
        (let ((doc (document store)))
          (register-document! (next-document-id) doc))
        #f)))

(define (export-csv-file docID dest)
  (let ((doc (get-document docID)))
    (if doc
        (begin
          (write-csv-file (document.store doc) dest)
          docID)
        #f)))

(define (save-delectus-file docID dest)
  (let ((doc (get-document docID)))
    (if doc
        (begin
          (write-delectus-file (document.store doc) dest)
          docID)
        #f)))

;;; (define $path1 "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_16k.csv")
;;; (define $path2 "/Users/mikel/Desktop/zipcode_16k.delectus")
;;; (define $docid (import-csv-file $path1 #t))
;;; (save-delectus-file $docid $path2)
;;; (define $docid2 (load-delectus-file $path2))
;;; (length (document.rows (%ensure-document $docid2 #f)))
;;; (length (get-filtered-rows (%ensure-document $docid2 #f)))
;;; (list-ref (get-filtered-rows (%ensure-document $docid2 #f)) 0)
;;; (document.filter-string (%ensure-document $docid2 #f))
;;; (document.set-filter-string! (%ensure-document $docid2 #f) "Por")
;;; (document.set-filter-string! (%ensure-document $docid2 #f) #f)
;;; (document.invalidate! (%ensure-document $docid2 #f))
;;; (begin (add-row! (%ensure-document $docid2 #f)) #t)
;;; (begin (document.set-sort-column! (%ensure-document $docid2 #f) "zip"))

;;; (define $path3 "/Users/mikel/Projects/delectus/releases/Delectus 10b9/test data/junior-movies.delectus")
;;; (define $docid3 (load-delectus-file $path3))
;;; (list-ref (get-filtered-rows (%ensure-document $docid3 #f)) 0)
;;; (document.set-filter-string! (%ensure-document $docid3 #f) "F")
;;; (document.invalidate! (%ensure-document $docid3 #f))
