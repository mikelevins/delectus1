;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          views.scm
;;;; Project:       Delectus
;;;; Purpose:       computed views of delectus tables
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (%make-row-comparator tbl sort-column sort-order)
  (let* ((col-index (table:column-index tbl sort-column))
         (numeric-column? (table:numeric-column? tbl sort-column))
         (accessor (if numeric-column?
                       (lambda (row)(row:element-as-number row col-index))
                       (lambda (row)(row:element row col-index))))
         (compare (if numeric-column?
                      ;; numeric sort
                      (if (= sort-order $SORT_ASCENDING)
                          <
                          (if (= sort-order $SORT_DESCENDING)
                              >
                              (always #t)))
                      ;; alphabetical sort
                      (if (= sort-order $SORT_ASCENDING)
                          string-ci<?
                          (if (= sort-order $SORT_DESCENDING)
                              string-ci>?
                              (always #t))))))
    (lambda (row1 row2)(compare (accessor row1) (accessor row2)))))

(define (view:create tbl #!key
                     (include-deleted #f)
                     (filter-text #f)
                     (sort-column #f)
                     (sort-order #f))
  (let* ((view-columns (if include-deleted
                           (%table-select-columns tbl (always #t))
                           (%table-select-columns tbl (complement column:deleted?))))
         (view-column-indexes (map (partial table:column-index tbl)
                                   (map column:label view-columns)))
         (live-rows (if include-deleted
                        (%table-select-rows tbl (always #t))
                        (%table-select-rows tbl (complement row:deleted?))))
         (view-rows (map (lambda (r)(row:make-with-row-entries r view-column-indexes))
                         live-rows))
         (filtered-rows (if filter-text
                            (filter (lambda (row)(row:match-text? row filter-text))
                             view-rows)
                            view-rows))
         (sorted-rows (if sort-column
                          (sort filtered-rows (%make-row-comparator tbl sort-column sort-order))
                          filtered-rows)))
    (%make-delectus-table (%make-column-sequence (list->vector view-columns))
                          (list->vector sorted-rows))))

;;; (define $cols '("Name" "Shape" "Color"))
;;; (define $rows '(("Fred" "Big" "Orange")("Barney" "Small" "Brown")("Wilma" "Slender" "White")))
;;; (define $d (table:make columns: $cols rows: $rows))
;;; (define $v1 (view:create $d filter-text: "w" sort-column: "Name" sort-order: $SORT_DESCENDING))
;;; (define $v2 (view:create $d filter-text: "i" sort-column: "Name" sort-order: $SORT_ASCENDING))