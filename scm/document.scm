;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.scm
;;;; Project:       Delectus
;;;; Purpose:       support for document behavior in DBs
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; About
;;; ======================================================================
;;; The Document API is the API that the Cocoa UI uses to interact
;;; with the DB back-end.
;;;
;;; The Delectus DB layer stores data. The Document layer serves as a
;;; view onto the DB. It preserves the sort and filter preferences
;;; expressed by the user, and caches the results of sorting and
;;; filtering, for the sake of efficient display.

;;; ======================================================================
;;; PRIVATE
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define next-document-id
  (let ((untitled-count 0))
    (lambda ()
      (let* ((i untitled-count))
        (set! untitled-count (+ untitled-count 1))
        i))))

;;; ----------------------------------------------------------------------
;;; document data structure
;;; ----------------------------------------------------------------------

(define-structure document 
  store
  valid?
  filter-string
  filtered-rows)

;;; ----------------------------------------------------------------------
;;; the global table of active documents
;;; ----------------------------------------------------------------------

(define $documents (make-table test: =))

;;; ======================================================================
;;; PUBLIC
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; document accessors
;;; ----------------------------------------------------------------------

;;; document fields

(define (document.store doc)(document-store doc))
(define (document.set-store! doc s)(document-store-set! doc s))

(define (document.valid? doc)(document-valid? doc))
(define (document.validate! doc)(document-valid?-set! doc #t))
(define (document.invalidate! doc)(document-valid?-set! doc #f))


(define (document.filter-string doc)(document-filter-string doc))
(define (document.set-filter-string! doc s)(document-filter-string-set! doc s))

(define (document.filtered-rows doc)(document-filtered-rows doc))
(define (document.set-filtered-rows! doc rs)(document-filtered-rows-set! doc rs))

;;; store fields

(define (document.store-version doc)(store-version (document.store doc)))
(define (document.set-store-version! doc v)(store-version-set! (document.store doc) v))

(define (document.columns doc)(store-columns (document.store doc)))
(define (document.set-columns! doc cs)(store-columns-set! (document.store doc) cs))

(define (document.get-column-labels doc)(store.get-column-labels (document.store doc)))

(define (document.show-deleted? doc)(store.show-deleted? (document.store doc)))
(define (document.show-deleted! doc)(store.set-show-deleted! (document.store doc) #t))
(define (document.hide-deleted! doc)(store.set-show-deleted! (document.store doc) #f))
(define (document.toggle-show-deleted! doc)
  (store.set-show-deleted! (document.store doc) (not (document.show-deleted? doc))))

(define (document.column-layout doc)(store.column-layout (document.store doc)))
(define (document.set-column-layout! doc co)(store.set-column-layout! (document.store doc) co))

(define (document.column-width doc label)(store.column-width (document.store doc) label))
(define (document.set-column-width! doc label new-width) (store.set-column-width! (document.store doc) label new-width))

(define (document.move-column! doc label new-index) (store.move-column! (document.store doc) label new-index))

(define (document.window-layout doc)(store.window-layout (document.store doc)))
(define (document.set-window-layout! doc co)(store.set-window-layout! (document.store doc) co))

(define (document.sort-column doc)(store-sort-column (document.store doc)))
(define (document.set-sort-column! doc label)(store-sort-column-set! (document.store doc) label))

(define (document.sort-reversed? doc)(store-sort-reversed? (document.store doc)))
(define (document.set-sort-reversed! doc reversed?)(store-sort-reversed?-set! (document.store doc) reversed?))

(define (document.rows doc)(store-rows (document.store doc)))
(define (document.set-rows! doc rs)(store-rows-set! (document.store doc) rs))

(define (document.notes doc)(store-notes (store-notes doc)))
(define (document.set-notes! doc n)(store-notes-set! (document.store doc) n))

;;; ----------------------------------------------------------------------
;;; obtaining documents
;;; ----------------------------------------------------------------------

(define (document store 
                  #!key
                  (filter-string #f))
  (let ((doc (make-document store #f filter-string '())))
    ;; force initial generation of sort and filter functions and filtered rows
    (document.invalidate! doc)
    doc))

(define (get-document pathname)(table-ref $documents pathname #f))

(define (register-document! docID doc)
  (table-set! $documents docID doc)
  docID)

(define (unregister-document! docID)
  (let ((doc (get-document docID)))
    (if doc
        (begin
          (table-set! $documents docID)
          docID)
        #f)))

(define (get-new-document!)
  (let* ((s (make-store (current-store-format) '() #f '() '() #f #f '() ""))
         (p (next-document-id))
         (d (document s)))
    (register-document! p d)
    p))

;;; ----------------------------------------------------------------------
;;; filtered and sorted contents 
;;; ----------------------------------------------------------------------

(define $end-sort-string
  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

(define (%string->number-for-sort s)
  (if (eqv? s #f)
      $maximum-row-index
      (let ((s (strip-whitespace s)))
        (if (empty-string? s)
            $maximum-row-index
            (string->number s)))))

(define (%all-visible-entries-are-numbers? doc sort-index)
  (let* ((rows (filter (if (document.show-deleted? doc)
                           id
                           (lambda (r) (not (row.deleted? r))))
                       (document.rows doc))))
    (every? (lambda (r)
              (%string->number-for-sort (field.value (list-ref (row.fields r) sort-index))))
            rows)))

(define (%string-numeric<? s1 s2)
  (< (%string->number-for-sort s1)
     (%string->number-for-sort s2)))

(define (%string-numeric>? s1 s2)
  (> (%string->number-for-sort s1)
     (%string->number-for-sort s2)))

(define (%normalize-value v)
  (if (string? v)
      (let ((v (strip-whitespace v)))
        (if (empty-string? v)
            $end-sort-string
            v))
      $end-sort-string))

(define (%string-ci-value>? v1 v2)
  (string-ci>? (%normalize-value v1)
               (%normalize-value v2)))

(define (%string-ci-value<? v1 v2)
  (string-ci<? (%normalize-value v1)
               (%normalize-value v2)))

(define (%row-visible-fields doc row)
  (let* ((visible-columns (get-visible-columns doc))
         (visible-indexes (map (lambda (c)
                                 (store:column-label->index (document.store doc) 
                                                            (column.label c)))
                               visible-columns))
         (fields (row.fields row)))
    (map (lambda (i) (list-ref fields i))
         visible-indexes)))

(define (%make-document-filter-function doc)
  (let* ((filter-string (document.filter-string doc))
         (string-filter (if filter-string
                            (let* ((field-matches? (lambda (f) (string-contains-ci (or (field.value f) "") filter-string)))
                                   (matches-filter? (lambda (r) (any? field-matches? (%row-visible-fields doc r)))))
                              (lambda (rows) (filter (lambda (r) (matches-filter? r)) rows)))
                            id))
         (show-deleted? (document.show-deleted? doc))
         (deleted-filter (if show-deleted?
                             id
                             (lambda (rows) (filter (lambda (r) (not (row.deleted? r))) rows)))))
    (lambda (rows) (deleted-filter (string-filter rows)))))

(define (%make-document-sort-function doc)
  (let* ((sort-label (document.sort-column doc)))
    (if sort-label
        (let* ((sort-index (store:column-label->index (document.store doc) sort-label))
               (sort-reversed? (document.sort-reversed? doc))
               (all-are-numbers? (%all-visible-entries-are-numbers? doc sort-index))
               (comparator (if all-are-numbers?
                               (if sort-reversed? %string-numeric>? %string-numeric<?)
                               (if sort-reversed? %string-ci-value>? %string-ci-value<?))))
          (lambda (rows)
            (sort rows
                  (lambda (r1 r2)
                    (comparator (field.value (list-ref (row.fields r1) sort-index))
                                (field.value (list-ref (row.fields r2) sort-index)))))))
        id)))

(define (document.regenerate-filtered-rows doc)
  (let* ((filter-fn (%make-document-filter-function doc))
         (sort-fn (%make-document-sort-function doc)))
    (document.set-filtered-rows! doc (sort-fn (filter-fn (document.rows doc))))
    (document.validate! doc)
    doc))

(define (get-filtered-rows doc)
  (if (not (document.valid? doc))
      (document.regenerate-filtered-rows doc))
  (document.filtered-rows doc))

;;; ----------------------------------------------------------------------
;;; info about documents 
;;; ----------------------------------------------------------------------

(define (%find-column label cols)
  (any? (lambda (c) (string-ci=? label (column-label c)))
        cols))

(define (contains-column? doc label)
  (%find-column label (document.columns doc)))

(define (get-visible-columns doc)
  (if (document.show-deleted? doc)
      (document.columns doc)
      (filter (lambda (c) (not (column.deleted? c)))
              (document.columns doc))))

(define (contains-visible-column? doc label)
  (%find-column label (get-visible-columns doc)))

(define (has-deleted-items? doc)
  (or (any? column.deleted? (document.columns doc))
      (any? row.deleted? (document.rows doc))))

;;; ----------------------------------------------------------------------
;;; document mutators 
;;; ----------------------------------------------------------------------

(define (set-filter! doc filter-string)
  (begin
    (document.set-filter-string! doc filter-string)
    (document.invalidate! doc)
    doc))

;;; cycle the column's sort state through:
;;; (1) no sort; (2) sort forward; (3) sort reversed
(define (advance-sort! doc label)
  (let ((current-sort (document.sort-column doc)))
    (if (and (string? current-sort)
             (string-ci=? label current-sort))
        ;; the current sort is the same column as the new sort
        (if (document.sort-reversed? doc)
            ;; if the current sort is reversed, then the next sort is no sort
            (begin
              (document.set-sort-column! doc #f)
              (document.set-sort-reversed! doc #f)
              (document.invalidate! doc)
              #f)
            ;; otherwise, the next sort is reversed
            (begin
              (document.set-sort-reversed! doc #t)
              (document.invalidate! doc)
              #t))
        ;; the current sort and the new sort are different
        ;; the new sort-state therefore is forward sort with the new column
        (begin
          (document.set-sort-column! doc label)
          (document.set-sort-reversed! doc #f)
          (document.invalidate! doc)
          #t))))

(define (add-column! doc label)
  (let ((already-col (contains-column? doc label)))
    (if already-col
        ;; if there's already a column with this label, but it's deleted, undelete it
        ;; also, reset the label, to make sure letter cases are the same as the input
        (if (column.deleted? already-col)
            (begin
              (column.set-label! already-col label)
              (column.set-deleted! already-col #f)
              (document.invalidate! doc)
              doc)
            ;; if the existing one is not deleted, then it's an error to try to add it
            (report-error context: (format "(add-column! ~a \"~a\")" doc label)
                          message: (format "The document already has a column labeled |'~a|'" label)
                          error: #f))
        ;; no existing column with that label; add one
        (begin
          ;; add the new column at the end. we always add the new column at the end, so that we
          ;; can simply add a new field at the end of each row to correspond to it
          (document.set-columns! doc (append (document.columns doc)
                                             (list (make-column label #f))))
          ;; now we have to add a new field at the end of each row to correspond to the new column
          (let loop ((rows (document.rows doc)))
            (if (null? rows)
                #f
                (let ((r (car rows))
                      (remaining-rows (cdr rows)))
                  (row.set-fields! r (append (row.fields r)
                                             (list (make-field #f))))
                  (loop remaining-rows))))
          ;; now add the new column's label to the end of the column-layout
          (%regenerate-column-layout (document.store doc))
          (if (not (assoc label (document.column-layout doc)))
              (document.set-column-layout! doc (append (document.column-layout doc)
                                                       (list (cons label $default-column-width)))))
          ;; now tell the document its view is invalid
          (document.invalidate! doc)
          doc))))

(define (rename-column! doc old-label new-label)
  (let ((col (store:column-label->column (document.store doc) old-label)))
    (store.layout-replace-column-name! (document.store doc) old-label new-label)
    (column.set-label! col new-label)
    (document.set-sort-column! doc #f)
    (document.set-sort-reversed! doc #f)
    (document.invalidate! doc)))

(define (add-row! doc)
  (let* ((make-item (lambda (x) #f))
         (vals (map make-item (document.columns doc)))
         (fields (map make-field vals))
         (row (make-row fields #f)))
    (document.set-rows! doc (append (document.rows doc)
                               (list row)))
    ;; when adding a row, we need to turn off the filter and sort, or the added row will be invisible
    (document.set-filter-string! doc #f)
    (document.set-sort-column! doc #f)
    (document.invalidate! doc)
    doc))

(define (toggle-column-deleted! doc label)
  (let ((found-column (contains-column? doc label)))
    (if found-column
        (begin
          (column.set-deleted! found-column (not (column.deleted? found-column)))
          (document.invalidate! doc))
        (report-error context: (format "(toggle-column-deleted! \"~a\" \"~a\")" doc label)
                      message: (format "No such column \"~a\" (when deleting a column)" label)
                      error: #f))))

(define (toggle-row-deleted! doc row-index)
  (let ((found-row (list-ref (get-filtered-rows doc)
                             row-index)))
    (begin
      (row.set-deleted! found-row (not (row.deleted? found-row)))
      (document.invalidate! doc))))

(define (toggle-show-deleted! doc)
  (document.toggle-show-deleted! doc)
  (document.invalidate! doc))

;;; we copy non-deleted rows from the old rows to the new.
;;; we copy each row by copying only those fields that are not
;;; in deleted columns. After the new rows and columns have been constructed,
;;; they replace the old ones in the store.
(define (empty-trash! doc)
  (%regenerate-column-layout (document.store doc))
  (let* ((store (document.store doc))
         (old-columns (store-columns store))
         (old-rows (store-rows store))
         (live-columns (filter (complement column.deleted?) old-columns))
         (live-column-labels (map column.label live-columns))
         (live-column-indexes (map (partial store:column-label->index store) live-column-labels))
         (live-rows (filter (complement row-deleted?) old-rows))
         (copy-row (lambda (r) 
                     (let ((fields (map (lambda (index) (list-ref (row.fields r) index))
                                        live-column-indexes)))
                       (make-row fields #f))))
         (new-rows (map copy-row live-rows)))
    (store.set-columns! store live-columns)
    (store.set-column-layout! store (filter (lambda (e) (member (car e) live-column-labels))
                                            (store.column-layout store)))
    (store.set-rows! store new-rows)
    (document.hide-deleted! doc)
    (document.invalidate! doc)
    doc))

;;; ======================================================================
;;; support for Data source methods in class DelectusDocument
;;; ======================================================================
;;; the rows used in the below functions are always filtered rows
;;; that is, they are the rows obtained after any filter and sort is applied
;;; not the complete list of rows

(define (count-visible-columns doc)(length (get-visible-columns doc)))

(define (count-filtered-rows doc)(length (get-filtered-rows doc)))

(define (value-for-cell doc column-label row-index)
  (let* ((col-index (store:column-label->index (document-store doc) column-label))
         (rows (get-filtered-rows doc))
         (row (list-ref rows row-index))
         (field (list-ref (row.fields row) col-index)))
    (field.value field)))

(define (set-value-for-cell! doc column-label row-index val)
  (let* ((col-index (store:column-label->index (document-store doc) column-label))
         (rows (get-filtered-rows doc))
         (row (list-ref rows row-index))
         (field (list-ref (row-fields row) col-index)))
    (field.set-value! field val)))

