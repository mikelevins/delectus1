;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          documents.scm
;;;; Project:       Delectus
;;;; Purpose:       document API for Delectus data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; the documents API is the primary means of communication between
;;; UI components and the engine

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define next-document-id
  (let ((next-doc-id 0))
    (lambda ()
      (set! next-doc-id (+ 1 next-doc-id))
      next-doc-id)))

;;; ----------------------------------------------------------------------
;;; document data structure
;;; ----------------------------------------------------------------------

(define-type document 
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-document
  (table doc:table)
  (view doc:view doc:set-view!)
  (view-valid doc:view-valid? doc:set-view-valid!)
  (view-description doc:view-description doc:set-view-description!)
  (include-deleted? doc:include-deleted? doc:set-include-deleted!)
  (filter-text doc:filter-text doc:set-filter-text!)
  (sort-column doc:sort-column doc:set-sort-column!)
  (sort-order doc:sort-order doc:set-sort-order!))

(define (doc:make #!key
                  (table (table:make))
                  (view #f)
                  (view-valid #f)
                  (view-description (view:null-description))
                  (include-deleted #f)
                  (filter-text #f)
                  (sort-column #f)
                  (sort-order #f))
  (%make-document table view view-valid view-description include-deleted
                  filter-text sort-column sort-order))

(define (update-view! doc)
  (or (doc:view-valid? doc)
      (begin
        (doc:set-view! doc
                       (view:create (doc:table doc)
                                    description: (doc:view-description doc)))
        (doc:set-view-valid! doc #t))))

;;; ----------------------------------------------------------------------
;;; the global table of active documents
;;; ----------------------------------------------------------------------

(define $documents (make-table test: =))

(define (find-document docid)
  (table-ref $documents docid #f))

(define (doc:register! docid doc)
  (table-set! $documents docid doc)
  docid)

;;; ----------------------------------------------------------------------
;;; API
;;; ----------------------------------------------------------------------

(define (new-document)
  (doc:register! (next-document-id)(doc:make)))

(define (get-view docid #!key (description (view:null-description)))
  (let ((doc (find-document docid)))
    (if doc
        (let ((doc-desc (doc:view-description doc)))
          (if (view:description-equal? description doc-desc)
              docid
              (begin
                (doc:set-view-description! doc description)
                (doc:set-view-valid! doc #f)
                docid)))
        (error "No such document"))))

(define (count-columns docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (table:count-columns tbl)))
        (error "No such document"))))

(define (count-rows docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (table:count-rows tbl)))
        (error "No such document"))))

(define (value-at docid column-label row-index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (row:element (table:row-at tbl row-index)
                         (table:column-index tbl column-label))))
        (error "No such document"))))

(define (put-value-at! docid column-label row-index val)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (row:set-element! (table:row-at tbl row-index)
                              (table:column-index tbl column-label)
                              val)
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (row-finished? docid row-index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (row:finished? (table:row-at tbl row-index))))
        (error "No such document"))))

(define (mark-row-finished! docid row-index finished?)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (row:set-finished! (table:row-at tbl row-index) 
                               (if finished? #t #f))
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (add-row! docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:table doc)))
            (table:add-row! tbl)
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (add-column! docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:table doc)))
            (table:add-column! tbl column-label)
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (column-deleted? docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((tbl (doc:view doc))
                 (col (table:column-at tbl column-label)))
            (column:deleted? col)))
        (error "No such document"))))

(define (mark-column-deleted! docid column-label deleted?)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((tbl (doc:view doc))
                 (col (table:column-at tbl column-label)))
            (column:set-deleted! col (if deleted? #t #f))
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))


