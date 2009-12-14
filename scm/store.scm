;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db.scm
;;;; Project:       Delectus
;;;; Purpose:       data storage for Delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; Basic data structures
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; constants
;;; ----------------------------------------------------------------------

(define $delectus-format-alpha-1 0)
(define $delectus-format-alpha-2 1)
(define $delectus-format-alpha-4 2)
(define $delectus-format-beta-2 3)

(define (current-store-format) $delectus-format-beta-2)

;;; a very large integer used mainly for sorting
(define $maximum-row-index 2000000000)

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define (store:vector-format? x tag)
  (and (vector? x)
       (> (vector-length x) 1)
       (eqv? tag (vector-ref x 0))))

;;; ----------------------------------------------------------------------
;;; fields
;;; ----------------------------------------------------------------------

(define-structure field
  value ; the value stored in the field
  )

;;; accessors

(define (field.value f)(field-value f))
(define (field.set-value! f v)(field-value-set! f v))

;;; serialization

(define (field.serialize f)
  (reporting-errors (lambda () 
                      (if (field? f)
                          (vector 'field (field.value f))
                          (abort (format "Not a field: ~a" f))))
                    context: (format "(serialize-field ~a)" f)
                    message: "Error serializing a field"
                    default: #f))

(define (serialized-field? f)(store:vector-format? f 'field))

(define (field.deserialize f)
  (reporting-errors (lambda () 
                      (if (serialized-field? f)
                          (make-field (vector-ref f 1))
                          (abort (format "Not a serialized field: ~a" f))))
                    context: (format "(deserialize-field ~a)" f)
                    message: "Error deserializing a field"
                    default: #f))

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------

(define-structure row
  fields ; the list of fields in the row
  deleted? ; whether this row has been deleted by the user
  )

;;; accessors

(define (row.fields r)(row-fields r))
(define (row.set-fields! r fs)(row-fields-set! r fs))
(define (row.deleted? r)(row-deleted? r))
(define (row.set-deleted! r deleted?)(row-deleted?-set! r deleted?))

;;; serialization

(define (row.serialize r)
  (reporting-errors (lambda () 
                      (if (row? r)
                          (vector 'row
                                  (map field.serialize (row.fields r))
                                  (row.deleted? r))
                          (abort (format "Not a row: ~a" r))))
                    context: (format "(row.serialize ~a)" r)
                    message: "Error serializing a row"
                    default: #f))

(define (serialized-row? r)(store:vector-format? r 'row))

(define (row.deserialize r)
  (reporting-errors (lambda () 
                      (if (serialized-row? r)
                          (make-row (map field.deserialize (vector-ref r 1))
                                    (vector-ref r 2))
                          (abort (format "Not a serialized row: ~a" r))))
                    context: (format "(deserialize-row ~a)" r)
                    message: "Error deserializing a row"
                    default: #f))

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define-structure column
  label ; the string used to label the column
  deleted? ; whether this column has been deleted by the user
  )

;;; used by layout code
(define $default-column-width 160.0)
(define $default-window-width 600.0)
(define $default-window-height 400.0)

;;; accessors

(define (column.label col)(column-label col))
(define (column.set-label! col lbl)(column-label-set! col lbl))
(define (column.deleted? col)(column-deleted? col))
(define (column.set-deleted! col deleted?)(column-deleted?-set! col deleted?))

;;; serialization

(define (column.serialize c)
  (reporting-errors (lambda () 
                      (if (column? c)
                          (vector 'column
                                  (column.label c)
                                  (column.deleted? c))
                          (abort (format "Not a column: ~a" c))))
                    context: (format "(serialize-column ~a)" c)
                    message: "Error serializing a column"
                    default: #f))

(define (serialized-column? c)(store:vector-format? c 'column))

(define (column.deserialize c)
  (reporting-errors (lambda () 
                      (if (serialized-column? c)
                          (make-column (vector-ref c 1)
                                       (vector-ref c 2))
                          (abort (format "Not a serialized column: ~a" c))))
                    context: (format "(deserialize-column ~a)" c)
                    message: "Error deserializing a column"
                    default: #f))

;;; ----------------------------------------------------------------------
;;; stores
;;; ----------------------------------------------------------------------

(define-structure store 
  version ; store format version
  columns ; all columns in the store
  show-deleted?
  column-layout ; ((label1 . width1)(label1 . width2)...)
  window-layout ; (left top width height)
  sort-column ; the column used for sorting the rows
  sort-reversed? ; whether to sort in reverse order
  rows ; all rows in the store
  notes ; user-supplied notes about the store
  )

(define (%regenerate-column-layout s)
  (if (or (null? (store.column-layout s))
          (not (list? (store.column-layout s))))
      ;; if there's no column-layout, make one
      (store-column-layout-set! s (map (lambda (c) (cons (column.label c) $default-column-width))
                                       (store.columns s)))))

;;; accessors

(define (store.version s)(store-version s))
(define (store.set-version! s v)(store-version-set! s v))
(define (store.columns s)(store-columns s))
(define (store.set-columns! s cs)(store-columns-set! s cs))

(define (store.get-column-labels s)
  (begin
    (%regenerate-column-layout s)
    (map car (store.column-layout s))))

(define (store.show-deleted? s)(store-show-deleted? s))
(define (store.set-show-deleted! s cs)(store-show-deleted?-set! s cs))
(define (store.column-layout s)(store-column-layout s))
(define (store.set-column-layout! s co)(store-column-layout-set! s co))
(define (store.column-width s label)(get-key (store.column-layout s) label $default-column-width))

(define (store.set-column-width! s label new-width)
  (begin
    (%regenerate-column-layout s)
    (let ((entry (assoc label (store.column-layout s))))
      (if entry
          (set-cdr! entry new-width)
          ;; if there's no entry, then the UI is trying to update a
          ;; column that has not been added to the layout by
          ;; regenerating it, which means that somehow it didn't get
          ;; added to the store's list of columns
          (report-error context: `(store.set-column-width! ,s ,label ,new-width)
                        message: "Error updating columns width"
                        error: #f)))))

(define (store.move-column! s label new-index)
  (%regenerate-column-layout s)
  (let* ((max-index (- (length (store.columns s)) 1))
         (new-index (min new-index max-index))
         (old-layout (store.column-layout s))
         (labeled-entry (any? (lambda (e) (equal? label (car e)))
                              old-layout))
         (other-entries (filter (lambda (e) (not (equal? label (car e))))
                                old-layout))
         (new-layout (append (take new-index other-entries)
                             (list labeled-entry)
                             (drop new-index other-entries))))
    (store.set-column-layout! s new-layout)))

(define (store.layout-replace-column-name! s old-label new-label)
  (%regenerate-column-layout s)
  (store.set-column-layout! s (map (lambda (entry) (if (equal? old-label (car entry))
                                                       (cons new-label (cdr entry))
                                                       entry))
                                   (store.column-layout s))))

(define (store.window-layout s)(store-window-layout s))
(define (store.set-window-layout! s co)(store-window-layout-set! s co))
(define (store.sort-column s)(store-sort-column s))
(define (store.set-sort-column! s lbl)(store-sort-column-set! s lbl))
(define (store.sort-reversed? s)(store-sort-reversed? s))
(define (store.set-sort-reversed! s reversed?)(store-sort-reversed?-set! s reversed?))
(define (store.rows s)(store-rows s))
(define (store.set-rows! s rs)(store-rows-set! s rs))
(define (store.notes s)(store-notes s))
(define (store.set-notes! s ns)(store-notes-set! s ns))


;;; serialization

(define (store.serialize s)
  (reporting-errors (lambda () 
                      (if (store? s)
                          (vector 'store
                                  (store.version s)
                                  (map column.serialize (store.columns s))
                                  (store.show-deleted? s)
                                  (store.column-layout s)
                                  (store.window-layout s)
                                  (store.sort-column s)
                                  (store.sort-reversed? s)
                                  (map row.serialize (store.rows s))
                                  (store.notes s))
                          (abort (format "Not a store: ~a" s))))
                    context: (format "(serialize-store ~a)" s)
                    message: "Error serializing a store"
                    default: #f))

(define (serialized-store? s)(store:vector-format? s 'store))
(define (serialized-store-format s)(vector-ref s 1))

(define (store.deserialize s)
  (reporting-errors (lambda () 
                      (if (serialized-store? s)
                          (make-store (vector-ref s 1) ; version
                                      (map column.deserialize (vector-ref s 2)) ; columns
                                      (vector-ref s 3) ; show-deleted?
                                      (vector-ref s 4) ; column-layout
                                      (vector-ref s 5) ; window-layout
                                      (vector-ref s 6) ; sort-column
                                      (vector-ref s 7) ; sort-reversed?
                                      (map row.deserialize (vector-ref s 8)) ; rows
                                      (vector-ref s 9))
                          (abort (format "Not a serialized store: ~a" s))))
                    context: (format "(deserialize-store ~a)" s)
                    message: "Error deserializing a store"
                    default: #f))

;;; ----------------------------------------------------------------------
;;; accessors and utilities
;;; ----------------------------------------------------------------------

(define (store:column-label->index store label)
  (position-if (lambda (c) (string-ci=? label (column.label c)))
               (store.columns store)))

(define (store:column-label->column store label)
  (any? (lambda (c) (string-ci=? label (column.label c)))
        (store.columns store)))

;;; ----------------------------------------------------------------------
;;; debugging utilities
;;; ----------------------------------------------------------------------

(define (describe-store s)
  (display (str
            (format "~%store:")
            (format "~%  file-format version: ~a" (store.version s))
            (format "~%  column-labels: ")
            (str (map (lambda (c) 
                        (if (column.deleted? c)
                            (format "~a(D)," (column.label c))
                            (format "~a," (column.label c))))
                      (store.columns s)))
            (format "~%  show-deleted?: ~a" (store.show-deleted? s))
            (format "~%  column-layout: ~a" (store.column-layout s))
            (format "~%  window-layout: ~a" (store.window-layout s))
            (format "~%  sort-column: ~a" (store.sort-column s))
            (format "~%  sort-reversed?: ~a" (store.sort-reversed? s))
            (format "~%  row-count: ~a" (length (store.rows s)))
            (format "~%  notes: ~a" (store.notes s))))
  (newline))