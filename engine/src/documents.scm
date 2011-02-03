;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          documents.scm
;;;; Project:       Delectus
;;;; Purpose:       user-oriented documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; document data structure
;;; ----------------------------------------------------------------------

(define-type document
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-document
  (format-version doc:format-version)
  (table doc:table)
  (deleted-columns doc:deleted-columns doc:set-deleted-columns!)
  (deleted-rows doc:deleted-rows doc:set-deleted-rows!)
  (include-deleted? doc:include-deleted? doc:set-include-deleted!)
  (sort-column doc:sort-column doc:set-sort-column!)
  (sort-order doc:sort-order doc:set-sort-order!)
  (sort-type doc:sort-type doc:set-sort-type!)
  (filter-text doc:filter-text doc:set-filter-text!)
  (view-columns doc:view-columns doc:set-view-columns!)
  (view-rows doc:view-rows doc:set-view-rows!)
  (view-valid? doc:view-valid? doc:set-view-valid!))

;;; ----------------------------------------------------------------------
;;; cache management
;;; ----------------------------------------------------------------------

(define (%default-row-comparator row1 row2) #t)

(define (%live-column-indexes doc)
  (range-except (map (partial table:column-index (doc:table doc))
                     (doc:deleted-columns doc))
                0
                (table:count-columns (doc:table doc))))

(define (%live-row-indexes doc)
  (range-except (doc:deleted-rows doc)
                0
                (table:count-rows (doc:table doc))))

(define (%filter-row-indexes doc row-indexes)
  (let ((tx (or (doc:filter-text doc) ""))
        (rows (table:rows (doc:table doc))))
    (filter (lambda (i)(string-vector-contains? (vector-ref rows i) tx))
            row-indexes)))

(define (%compare-op sort-type sort-order #!key (default-comparator default-row-comparator))
  (cond
   ((= sort-order $SORT_ASCENDING) 
    (cond
     ((= sort-type $SORT_NUMERIC) <)
     ((= sort-type $SORT_ALPHABETICAL) string-ci<?)
     (else default-comparator)))
   ((= sort-order $SORT_DESCENDING)
    (cond
     ((= sort-type $SORT_NUMERIC) >)
     ((= sort-type $SORT_ALPHABETICAL) string-ci>?)
     (else default-comparator)))
   (else default-comparator)))

(define (%make-row-comparator doc col-index sort-type sort-order)
  (let ((comp (doc:compare-op sort-type sort-order)))
    (lambda (row-index1 row-index2)
      (comp (vector-ref (vector-ref (table:rows (doc:table doc)) row-index1) col-index)
            (vector-ref (vector-ref (table:rows (doc:table doc)) row-index2) col-index)))))

(define (%row-comparator doc)
  (let ((sort-column (doc:sort-column doc))
        (sort-order (doc:sort-order doc))
        (sort-type (doc:sort-order doc)))
    (if (and sort-column
             (and sort-order (not (= sort-order $SORT_NONE)))
             (and sort-type (not (= sort-type $SORT_NONE))))
        (doc:make-row-comparator doc
                                 (table:column-index (doc:table) sort-column)
                                 sort-type
                                 sort-order)
        default-row-comparator)))

(define (%sort-row-indexes doc row-indexes)
  (let ((comp (doc:row-comparator doc)))
    (sort row-indexes comp)))

(define (%update-view! doc)
  (if (not (view? (doc:view doc)))
      (doc:set-view! doc (view:make)))
  (if (not (doc:view-valid? doc))
      (let* ((live-cols (doc:live-column-indexes doc))
             (live-rows (doc:live-row-indexes doc))
             (filtered-rows (doc:filter-row-indexes doc live-rows))
             (sorted-rows (doc:sort-row-indexes doc filtered-rows)))
        (doc:set-view-columns! doc live-cols)
        (doc:set-view-rows! doc sorted-rows)
        (doc:set-view-valid! doc #t))))

;;; ----------------------------------------------------------------------
;;; table API
;;; ----------------------------------------------------------------------

(define (doc:make #!key 
                  (table #f)
                  (deleted-columns '())
                  (deleted-rows '())
                  (include-deleted? #f)
                  (sort-column #f)
                  (sort-order #f)
                  (sort-type #f)
                  (filter-text #f))
  (%make-document
   $delectus-format-1.0
   (or table (table:make))
   deleted-columns
   deleted-rows
   include-deleted?
   sort-column
   sort-order
   sort-type
   filter-text
   #f
   #f))

;;; the table API

(define (doc:column-at doc column-index)
  )

(define (doc:column-index del label)
  )

(define (doc:count-columns doc)
  )

(define (doc:count-rows doc)
  )

(define (doc:add-row! doc)
  (table:add-row! (doc:table doc))
  (doc:set-view-valid! doc #f)
  doc)

(define (doc:add-column! doc label)
  (table:add-column! (doc:table doc) label)
  (doc:set-view-valid! doc #f)
  doc)

(define (doc:value-at doc column-label row-index)
  (doc:update-view! doc)
  ;; TODO: implement!
  )

(define (doc:put-value-at! doc column-label row-index val)
  (doc:update-view! doc)
  ;; TODO: implement!
  
  (doc:set-view-valid! doc #f)
  doc)

;;; document-specific API

(define (doc:column-deleted? doc column-label)
  (contains? string-ci=? (doc:deleted-columns doc) column-label))

(define (doc:column-index doc column-label)
  (doc:update-view! doc)
  (if (or (doc:show-deleted? doc)
          (not (doc:column-deleted? doc column-label)))
      (table:column-index (doc:table doc) column-label)
      #f))

(define (doc:mark-column-deleted! doc column-label deleted?)
  (if deleted?
      (begin
        (if (not (doc:column-deleted? doc column-label))
            (doc:set-deleted-columns! doc (cons column-label (doc:deleted-columns doc))))
        (doc:set-view-valid! doc #f)
        doc)
      (begin
        (doc:set-deleted-columns! doc (remove column-label (doc:deleted-columns doc) test: string-ci=?))
        (doc:set-view-valid! doc #f)
        doc)))

(define (doc:row-deleted? doc row-index)
  (contains? = (doc:deleted-rows doc) row-index))

(define (doc:mark-row-deleted! doc row-index deleted?)
  (if deleted?
      (begin
        (if (not (doc:row-deleted? doc row-index))
            (doc:set-deleted-rows! doc (cons row-index (doc:deleted-rows doc))))
        (doc:set-view-valid! doc #f)
        doc)
      (begin
        (doc:set-deleted-rows! doc (remove row-index (doc:deleted-rows doc) test: =))
        (doc:set-view-valid! doc #f)
        doc)))

(define (doc:show-deleted? doc)
  (doc:include-deleted? doc))

(define (doc:set-show-deleted! doc deleted?)
  (doc:set-include-deleted! doc deleted?)
  (doc:set-view-valid! doc #f))

(define (doc:compact-table! doc)
  (table:remove-rows! (doc:table doc)
                      (doc:deleted-rows doc))
  (table:remove-columns! (doc:table doc)
                         (doc:deleted-columns doc))
  (doc:set-deleted-rows! doc '())
  (doc:set-deleted-columns! doc '())
  (doc:set-view-valid! doc #f))

