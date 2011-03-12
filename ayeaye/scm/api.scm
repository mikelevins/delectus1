;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.scm
;;;; Project:       Delectus
;;;; Purpose:       scheme functions that implement the API used by the 
;;;;                c-interface
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; global constants
;;; ----------------------------------------------------------------------

(define $no-document -1)

;;; ----------------------------------------------------------------------
;;; document utilities
;;; ----------------------------------------------------------------------

(define (%ensure-document document-id context)
  (let ((doc (get-document document-id)))
    (or doc
        (begin
          (report-error context: context
                        message: (format "No document found for ID \"~a\"" document-id)
                        error: #f)
          #f))))

(define (%with-valid-document document-id proc #!key (context #f)(message #f)(default #f))
  (reporting-errors
   (lambda ()
     (let* ((doc (%ensure-document document-id context)))
       (if doc
           (proc doc)
           (error "Invalid document"))))
   context: context
   message: message
   default: default))

;;; ----------------------------------------------------------------------
;;; support for NSDocument APIs in class DelectusDocument
;;; ----------------------------------------------------------------------

(define (api:read-csv-from-path pathname headers-in-first-line?)
  (reporting-errors
   (lambda ()
     (or (import-csv-file pathname headers-in-first-line?)
         (error "Read failed")))
   context: `(api:read-csv-from-path ,pathname ,headers-in-first-line?)
   message: "I/O error"
   default: $no-document))

(define (api:read-delectus-from-path pathname)
  (reporting-errors
   (lambda ()
     (or (load-delectus-file pathname)
         (error "Read failed")))
   context: `(api:read-delectus-from-path ,pathname)
   message: "I/O error"
   default: $no-document))

(define (api:write-csv-to-path docID dest)
  (reporting-errors
   (lambda ()
     (or (export-csv-file docID dest)
         (error "Write failed")))
   context: `(api:write-csv-to-path ,docID ,dest)
   message: "I/O error"
   default: $no-document))

(define (api:write-delectus-to-path docID dest)
  (reporting-errors
   (lambda ()
     (or (save-delectus-file docID dest)
         (error "Write failed")))
   context: `(api:write-delectus-to-path ,docID ,dest)
   message: "I/O error"
   default: $no-document))

;;; (define $path1 "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $path2 "/Users/mikel/Desktop/zipcode.delectus")
;;; (define $docid (api:read-csv-from-path $path1 #t))
;;; (api:write-delectus-to-path $docid $path2)
;;; (define $docid2 (api:read-delectus-from-path $path2))
;;; (value-for-cell (%ensure-document $docid #f) "city" 100)
;;; (value-for-cell (%ensure-document $docid2 #f) "city" 100)

;;; ----------------------------------------------------------------------
;;; support for NSDocument IBActions in class DelectusDocument
;;; ----------------------------------------------------------------------

(define (api:set-filter document-id filter-string)
  (%with-valid-document
   document-id
   (lambda (doc)
     (set-filter! doc filter-string)
     (cocoa:notify-reload-data document-id))
   context: `(api:set-filter ,document-id ,filter-string)
   message: "Setting a document filter failed"
   default: (void)))

(define (api:advance-sort! document-id label)
  (%with-valid-document
   document-id
   (lambda (doc) 
     (let ((new-sort? (advance-sort! doc label)))
       (cocoa:notify-reload-data document-id)
       new-sort?))
   context: `(api:advance-sort! ,document-id ,label)
   message: "Updating the sort failed"
   default: #f))

(define (api:add-column document-id label)
  (%with-valid-document
   document-id
   (lambda (doc) 
     (add-column! doc label)
     (cocoa:notify-reload-data document-id))
   context: `(api:add-column ,document-id ,label)
   message: "Adding a column failed"
   default: (void)))

(define (api:rename-column! document-id old-label new-label)
  (%with-valid-document
   document-id
   (lambda (doc) 
     (rename-column! doc old-label new-label)
     (cocoa:notify-reload-data document-id))
   context: `(api:rename-column! ,document-id ,old-label ,new-label)
   message: "Renaming a column failed"
   default: (void)))

(define (api:add-row document-id)
  (%with-valid-document
   document-id
   (lambda (doc) 
     (add-row! doc)
     (cocoa:notify-reload-data document-id))
   context: `(api:add-row ,document-id)
   message: "Adding a row failed"
   default: (void)))

(define (api:toggle-column-deleted! document-id label)
  (%with-valid-document
   document-id
   (lambda (doc)
     (toggle-column-deleted! doc label)
     (cocoa:notify-reload-data document-id))
   context: `(api:toggle-column-deleted! ,document-id ,label)
   message: "Deleting or undeleting a column failed"
   default: (void)))

;;; the row is identified by index into the displayed rows. In other words, the indicated row
;;; is looked up in the filtered and sorted rows, not in the DB's list of all rows
(define (api:toggle-row-deleted! document-id row-index)
  (%with-valid-document
   document-id
   (lambda (doc)
     (toggle-row-deleted! doc row-index)
     (cocoa:notify-reload-data document-id))
   context: `(api:toggle-row-deleted! ,document-id ,row-index)
   message: "Deleting or undeleting a row failed"
   default: (void)))

(define (api:toggle-show-deleted! document-id)
  (%with-valid-document
   document-id
   (lambda (doc)
     (toggle-show-deleted! doc)
     (cocoa:notify-reload-data document-id))
   context: `(api:toggle-show-deleted! ,document-id)
   message: "Hiding or showing deleted items failed"
   default: (void)))

(define (api:empty-trash! document-id)
  (%with-valid-document
   document-id
   (lambda (doc)
     (empty-trash! doc)
     (cocoa:notify-reload-data document-id))
   context: `(api:empty-trash! ,document-id)
   message: "Emptying the trash failed"
   default: (void)))

;;; ----------------------------------------------------------------------
;;; support for Data source methods in class DelectusDocument
;;; ----------------------------------------------------------------------

;;; returns the number of columns currently shown (not necessarily the total
;;; number of columns, as deleted columns may be hidden)
(define (api:number-of-columns document-id)
  (%with-valid-document
   document-id
   (lambda (doc) (count-visible-columns doc))
   context: `(api:number-of-columns ,document-id)
   message: "Counting a document's columns failed"
   default: 0))

;;; returns a count of the filtered and sorted rows (not a count of *all* rows)
(define (api:number-of-rows document-id)
  (%with-valid-document
   document-id
   (lambda (doc) (count-filtered-rows doc))
   context: `(api:number-of-rows ,document-id)
   message: "Counting a document's rows failed"
   default: 0))

(define (api:value-for-cell document-id column-label row-index)
  (%with-valid-document
   document-id
   (lambda (doc)
     (let ((val (value-for-cell doc column-label row-index)))
       (if val
           (bridge:string->ns-string val)
           (cocoa:null))))
   context: `(api:value-for-cell ,document-id ,column-label ,row-index)
   message: "Getting the contents of a cell failed"
   default: (cocoa:null)))

;;; the row is identified by index into the displayed rows. In other words, the indicated row
;;; is looked up in the filtered and sorted rows, not in the DB's list of all rows
(define (api:set-value-for-cell! document-id column-label row-index val)
  (%with-valid-document
   document-id
   (lambda (doc)(set-value-for-cell! doc column-label row-index val))
   context: `(api:set-value-for-cell! ,document-id ,column-label ,row-index ,val)
   message: "Setting the contents of a cell failed"
   default: (void)))

;;; ----------------------------------------------------------------------
;;; support for NSTableView display in class DelectusDocument
;;; ----------------------------------------------------------------------

(define (api:get-new-document)(get-new-document!))

(define (api:get-column-labels document-id)
  (%with-valid-document
   document-id
   (lambda (doc)
     (let* ((labels (document.get-column-labels doc))
            (result (bridge:string-list->nsstring-array labels)))
       result))
   context: `(api:get-column-labels ,document-id)
   message: "Getting the column labels failed"
   default: (cocoa:null)))

(define (api:is-column-deleted? document-id label)
  (%with-valid-document
   document-id
   (lambda (doc)
     (let ((col (store:column-label->column (document-store doc) label)))
       (if col
           (column-deleted? col)
           (error (str "No such column: " label)))))
   context: `(api:is-column-deleted? ,document-id ,label)
   message: "Failed when checking whether a column is deleted"
   default: #t))

(define (api:get-column-width document-id label)
  (%with-valid-document
   document-id
   (lambda (doc)(document.column-width doc label))
   context: `(api:get-column-width ,document-id ,label)
   message: "Failed getting a column's width"
   default: $default-column-width))

(define (api:set-column-width! document-id label new-width)
  (%with-valid-document
   document-id
   (lambda (doc) (document.set-column-width! doc label new-width))
   context: `(api:set-column-width! ,document-id ,label ,new-width)
   message: "Failed setting a column's width"
   default: (void)))

;;; TODO: implement this
(define (api:move-column! document-id label new-index)
  (%with-valid-document
   document-id
   (lambda (doc) (document.move-column! doc label new-index))
   context: `(api:move-column! ,document-id ,label ,new-index)
   message: "Failed moving a column"
   default: (void)))

;;; row-index refers to the index in the *filtered* rows, not all the document's rows
(define (api:is-row-deleted? document-id row-index)
  (%with-valid-document
   document-id
   (lambda (doc)
     (let ((row (list-ref (get-filtered-rows doc) row-index)))
       (if row
           (row-deleted? row)
           (error (format "No visible row ~a" row-index)))))
   context: `(api:is-row-deleted? ,document-id ,row-index)
   message: "Failed when checking whether a row is deleted"
   default: #t))

(define (api:are-deleted-items-shown? document-id)
  (%with-valid-document
   document-id
   (lambda (doc)(document.show-deleted? doc))
   context: `(api:are-deleted-items-shown? ,document-id)
   message: "Failed when determining whether deleted items are shown"
   default: #f))

(define (api:has-deleted-items? document-id)
  (%with-valid-document
   document-id
   (lambda (doc)(has-deleted-items? doc))
   context: `(api:has-deleted-items? ,document-id)
   message: "Failed when determining whether a document has deleted items"
   default: #f))

(define (api:is-duplicate-label? document-id label)
  (%with-valid-document
   document-id
   (lambda (doc) (or (and (contains-visible-column? doc label)
                          #t)
                     #f))
   context: `(api:is-duplicate-label? ,document-id ,label)
   message: "Failed when determining whether a column label is a duplicate"
   default: #t))

