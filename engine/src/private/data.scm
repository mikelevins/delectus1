;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          data.scm
;;;; Project:       Delectus
;;;; Purpose:       base data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; entries
;;; ----------------------------------------------------------------------

(define-type entry
  id: DB7F49A3-BBF6-40F3-95A1-038CCACE1D88
  constructor: %make-entry
  (value entry:value %set-entry-value!)
  (number-value %entry-number-value %set-entry-number-value!))

(define (%->entry-number-value thing)
  (if (string? thing)
      (string->number thing)
      (if (null? thing)
          #f
          (error "Invalid entry value" thing))))

(define (entry:make val)
  (%make-entry val (%->entry-number-value val)))

(define (entry:set-value! e val)
  (%set-entry-value! e val)
  (%set-entry-number-value! e (%->entry-number-value val))
  val)

;;; (define $e (entry:make "zero"))
;;; (entry:value $e)
;;; (%entry-number-value $e)
;;; (entry:set-value! $e "0")
;;; (entry:value $e)
;;; (%entry-number-value $e)

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------

(define-type row
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-row
  (entries row:entries row:set-entries!)
  (deleted? row:deleted? row:set-deleted!))

(define (row:make vals)
  (%make-row (list->vector (map entry:make vals)) #f))

(define (row:make-with-row-entries r indexes)
  (let loop ((is indexes)
             (result '()))
    (if (null? is)
        (%make-row (list->vector (reverse result)) 
                   (row:deleted? r))
        (loop (cdr is)
              (cons (vector-ref (row:entries r) (car is))
                    result)))))

(define (row:element seq index)
  (entry:value (vector-ref (row:entries seq) index)))

(define (row:element-as-number seq index)
  (%entry-number-value (vector-ref (row:entries seq) index)))

(define (row:position seq val)
  (vector-position (lambda (elt v)(string-ci=? (entry:value elt) v))
                   (row:entries seq) val))

(define (row:set-element! seq index val)
  (entry:set-value! (vector-ref (row:entries seq) index) val))

(define (row:add-element! seq val)
  (row:set-entries! seq (vector-add-last (row:entries seq) (entry:make val))))

;;; (define $s (row:make '("fred" "12" "bedrock")))
;;; (row:element $s 0)
;;; (row:element-as-number $s 0)
;;; (row:element-as-number $s 1)
;;; (row:position $s "bedrock")
;;; (row:position $s "foobar")
;;; (row:set-element! $s 0 "12.34")
;;; (row:element-as-number $s 0)
;;; (row:add-element! $s "fred")
;;; (row:element $s 3)

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define-type column
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-column
  (label column:label)
  (deleted? column:deleted? column:set-deleted!))

(define (column:make label #!key (deleted #f))
  (if (string? label)
      (%make-column label deleted)
      (error "Invalid column label" label)))

;;; (define $c (column:make "Name"))
;;; $c

;;; ----------------------------------------------------------------------
;;; column-sequences
;;; ----------------------------------------------------------------------

(define-type column-sequence
  id: 2D65FC83-52F9-4A24-BF8E-99A8CA106583
  constructor: %make-column-sequence
  (columns column-sequence:columns column-sequence:set-columns!))

(define (column-sequence:make labels)
  (if (duplicates? string-ci=? labels)
      (error "Duplicate column labels" labels)
      (%make-column-sequence (list->vector (map column:make labels)))))

(define (column-sequence:element seq index)
  (vector-ref (column-sequence:columns seq) index))

(define (column-sequence:position seq val)
  (vector-position (lambda (elt v)(string-ci=? (column:label elt) v))
                   (column-sequence:columns seq) val))

(define (column-sequence:length seq)
  (vector-length (column-sequence:columns seq)))

(define (column-sequence:add-element! seq val)
  (let ((val-index (column-sequence:position seq val)))
    (if val-index
        (error "Column exists" val)
        (column-sequence:set-columns! seq (vector-add-last (column-sequence:columns seq) (column:make val))))))

;;; (define $cs (column-sequence:make '("Name" "Age" "Town")))
;;; $cs
;;; (column-sequence:element $cs 0)
;;; (column-sequence:position $cs "Age")
;;; (column-sequence:add-element! $cs "Color")
;;; (column-sequence:position $cs "Color")
;;; (column-sequence:element $cs 3)

;;; ----------------------------------------------------------------------
;;; tables
;;; ----------------------------------------------------------------------

(define-type delectus-table
  id: D604C012-3A9D-4D7B-BE70-4ACB9260659F
  constructor: %make-delectus-table
  (column-sequence table:column-sequence table:set-column-sequence!)
  (rows table:rows table:set-rows!))

(define (%table-parse-columns column-descriptions)
  (column-sequence:make column-descriptions))

(define (%table-parse-rows row-descriptions)
  (list->vector (map row:make row-descriptions)))

;;; return a list of columns
(define (%table-select-columns tbl pred)
  (vector->list (vector-filter pred (column-sequence:columns (table:column-sequence tbl)))))

;;; return a list of rows
(define (%table-select-rows tbl pred)
  (vector->list (vector-filter pred (table:rows tbl))))

(define (table:make #!key (columns '()) (rows '()))
  (let ((col-sequence (%table-parse-columns columns))
        (row-vector (%table-parse-rows rows)))
    (%make-delectus-table col-sequence row-vector)))

(define (table:compact! tbl)
  (let* ((new-cols (%table-select-columns tbl (complement column:deleted?)))
         (live-column-labels (map column:label new-cols))
         (live-column-indexes (map (partial column-sequence:position (table:column-sequence tbl))
                                   live-column-labels))
         (live-rows (%table-select-rows tbl (complement row:deleted?)))
         (new-rows (map (lambda (r)(row:make-with-row-entries r live-column-indexes))
                        live-rows)))
    (table:set-column-sequence! tbl (column-sequence:make live-column-labels))
    (table:set-rows! tbl (list->vector new-rows))
    tbl))

(define (table:count-columns tbl)
  (column-sequence:length (table:column-sequence tbl)))

(define (table:count-rows tbl)
  (vector-length (table:rows tbl)))

(define (table:column-at tbl column-label)
  (let ((col-seq (table:column-sequence tbl)))
    (column-sequence:element col-seq
                             (column-sequence:position col-seq column-label))))

(define (table:row-at tbl row-index)
  (vector-ref (table:rows tbl) row-index))

(define (table:add-row! tbl)
  (table:set-rows! tbl
                   (vector-add-last (table:rows tbl)
                                    (row:make (repeat (table:count-columns tbl) '()))))
  tbl)

(define (table:add-column! tbl column-label)
  (column-sequence:add-element! (table:column-sequence tbl) column-label)
  (vector-for-each (lambda (row)(row:add-element! row '()))
            (table:rows tbl))
  tbl)

(define (table:mark-column-deleted! tbl column-label deleted?)
  (column:set-deleted! (table:column-at tbl column-label)
                       deleted?))

(define (table:mark-row-deleted! tbl row-index deleted?)
  (row:set-deleted! (table:row-at tbl row-index)
                       deleted?))

;;; (define $cols '("Name" "Shape" "Color"))
;;; (define $rows '(("Fred" "Big" "Orange")("Barney" "Small" "Brown")("Wilma" "Slender" "White")))
;;; (define $d (table:make columns: $cols rows: $rows))
;;; $d
;;; (table:count-columns $d)
;;; (table:row-at $d 1)
;;; (table:add-row! $d)
;;; $d
;;; (table:add-column! $d "Town")
;;; (table:count-columns $d)
;;; $d
;;; (table:mark-column-deleted! $d "Shape" #t)
;;; (column:deleted? (table:column-at $d "Shape"))
;;; (table:mark-column-deleted! $d "Shape" #f)
;;; (column:deleted? (table:column-at $d "Shape"))
;;; $d
;;; (table:mark-row-deleted! $d 2 #t)
;;; (row:deleted? (table:row-at $d 2))
;;; (table:mark-row-deleted! $d 2 #f)
;;; (row:deleted? (table:row-at $d 2))
;;; $d
;;; (table:count-rows $d)
;;; (table:mark-row-deleted! $d 2 #t)
;;; (table:compact! $d)
;;; (table:count-rows $d)
;;; $d
;;; (table:count-columns $d)
;;; (table:mark-column-deleted! $d "Shape" #t)
;;; (table:compact! $d)
;;; (table:count-columns $d)
;;; $d
