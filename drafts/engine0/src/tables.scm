;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-tables.scm
;;;; Project:       Delectus
;;;; Purpose:       basic data structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; Private
;;; ----------------------------------------------------------------------

(define-type delectus-table
  id: 045F1C7B-788D-4F47-A325-BDB0D60EF205
  constructor: %make-delectus-table
  (columns table:columns table:set-columns!)
  (rows table:rows table:set-rows!))

(define (%parse-rows rows)
  (list->vector
   (let loop ((i 0)
              (result '())
              (rows rows))
     (if (null? rows)
         (reverse result)
         (let* ((row (car rows))
                (vals (map delectus-value row)))
           (loop (+ i 1)
                 (cons (row:make elements: vals index: i)
                       result)
                 (cdr rows)))))))

(define (%delectus-column-label thing)
  (if (string? thing)
      thing
      (error "Invalid column label" thing)))

(define (%parse-columns columns)
  (let ((labels (map %delectus-column-label columns)))
    (if (duplicates? string-ci=? labels)
        (error "Duplicate labels" labels)
        (list->vector
         (let loop ((i 0)
                    (result '())
                    (labels labels))
           (if (null? labels)
               (reverse result)
               (let* ((label (car labels)))
                 (loop (+ i 1)
                       (cons (col:make label index: i)
                             result)
                       (cdr labels)))))))))

;;; ----------------------------------------------------------------------
;;; Public constructors
;;; ----------------------------------------------------------------------

(define (table:make 
         #!key
         (columns '())(rows '()))
  (let* ((rows (%parse-rows rows))
         (cols (%parse-columns columns)))
    (%make-delectus-table cols rows)))

;;; ----------------------------------------------------------------------
;;; Public access API
;;; ----------------------------------------------------------------------

(define (table:column-at tbl column-index)
  (vector-ref (table:columns tbl) column-index))

(define (table:column-index tbl column-label)
  (vector-position (lambda (col lbl)(string-ci=? (col:label col) lbl))
                   (table:columns tbl) 
                   column-label))

(define (table:count-columns tbl)
  (vector-length (table:columns tbl)))

(define (table:row-at tbl row-index)
  (vector-ref (table:rows tbl) row-index))

(define (table:count-rows tbl)
  (vector-length (table:rows tbl)))

(define (table:value-at tbl column-label row-index)
  (vector-ref (row:elements (vector-ref (table:rows tbl) row-index)) 
              (table:column-index tbl column-label)))

(define (table:put-value-at! tbl column-label row-index val)
  (let ((val (delectus-value val))
        (column-index (table:column-index tbl column-label)))
    (vector-set! (row:elements (vector-ref (table:rows tbl) row-index)) column-index val)
    tbl))

;;; ----------------------------------------------------------------------
;;; Public filtering API
;;; ----------------------------------------------------------------------

(define (table:column-indexes-matching table pred)
  (let ((colcount (table:count-columns table)))
    (let loop ((i 0)
          (result '()))
      (if (< i colcount)
          (let ((e (vector-ref (table:columns table) i)))
            (if (pred e)
                (loop (+ i 1)(cons i result))
                (loop (+ i 1) result)))
          (reverse result)))))

(define (table:columns-matching table pred)
  (vector-select (table:columns table)
                 (table:column-indexes-matching table pred)))

(define (table:row-indexes-matching table pred)
  (let ((rowcount (table:count-rows table)))
    (let loop ((i 0)
          (result '()))
      (if (< i rowcount)
          (let ((e (vector-ref (table:rows table) i)))
            (if (pred e)
                (loop (+ i 1)(cons i result))
                (loop (+ i 1) result)))
          (reverse result)))))

(define (table:rows-matching table pred)
  (vector-select (table:rows table)
                 (table:row-indexes-matching table pred)))

(define (table:select table #!key (column-test (always #t))(row-test (always #t)))
  (let* ((new-column-indexes (table:column-indexes-matching table column-test))
         (new-columns (map (lambda (i)(col:copy (table:column-at table i)))
                           new-column-indexes))
         (new-row-indexes (table:row-indexes-matching table row-test))
         (new-rows (map (lambda (i)(row:select-elements (table:row-at table i) new-column-indexes))
                        new-row-indexes))
         (new-row-count (length new-rows)))
    (let loop ((i 0)
               (rows new-rows))
      (if (not (null? rows))
          (begin
            (row:set-index! (car rows) i)
            (loop (+ i 1)(cdr rows)))))
    (%make-delectus-table (list->vector new-columns) (list->vector new-rows))))

;;; ----------------------------------------------------------------------
;;; Public restructuring API
;;; ----------------------------------------------------------------------

(define (table:add-row! tbl)
  (let* ((col-count (table:count-columns tbl))
         (row-count (table:count-rows tbl))
         (added-row (row:make elements: (repeat col-count (nothing)) index: row-count))
         (new-rows (vector-add-last (table:rows tbl) added-row)))
    (table:set-rows! tbl new-rows)))

(define (table:add-column! tbl label)
  (let ((already (table:column-index tbl label)))
    (if already
        (error "Column exists" label)
        (let* ((col (col:make label index: (table:count-columns tbl)))
               (rows (table:rows tbl))
               (row-count (vector-length rows)))
          (table:set-columns! tbl (vector-add-last (table:columns tbl) col))
          (let loop ((i 0))
            (if (>= i row-count)
                tbl
                (row:add-last! (vector-ref (table:rows tbl) i) (nothing))))))))

(define (table:mark-column-deleted! tbl label deleted?)
  (let ((col (table:column-at tbl (table:column-index tbl label))))
    (col:set-deleted! col deleted?)))

(define (table:mark-row-deleted! tbl row-index deleted?)
  (let ((row (table:row-at tbl row-index)))
    (row:set-deleted! row deleted?)))

(define (table:compact! tbl)
  (let ((new-table (table:select tbl 
                                 column-test: (complement col:deleted?)
                                 row-test: (complement row:deleted?))))
    (table:set-columns! tbl (table:columns new-table))
    (table:set-rows! tbl (table:rows new-table))
    tbl))

;;; ----------------------------------------------------------------------
;;; Public sorting API
;;; ----------------------------------------------------------------------

(define (table:make-row-comparator table column-index comparator)
  (lambda (row1 row2)
    (comparator (vector-ref (row:elements row1) column-index)
                (vector-ref (row:elements row2) column-index))))

