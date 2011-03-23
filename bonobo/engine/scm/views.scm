;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          views.scm
;;;; Project:       Delectus
;;;; Purpose:       computed views of delectus tables
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type view-description
  id: 8A68D921-4553-4F94-BC60-9D6624A123AD
  constructor: %make-view-description
  (include-deleted? view-description:include-deleted?)
  (filter-text view-description:filter-text)
  (sort-column view-description:sort-column)
  (sort-order view-description:sort-order))

(define (view:description #!key
                          (include-deleted #f)
                          (filter-text #f)
                          (sort-column #f)
                          (sort-order #f))
  (%make-view-description include-deleted
                          filter-text
                          sort-column
                          sort-order))

(define (view:default-description)
  (%make-view-description #f
                          #f
                          #f
                          #f))

(define (view:description-equal? desc1 desc2)
  (or (eqv? desc1 desc2)
      (and (eqv? (view-description:include-deleted? desc1)
                 (view-description:include-deleted? desc2))
           (or (and (not (view-description:filter-text desc1))
                    (not (view-description:filter-text desc2)))
               (string-ci=? (view-description:filter-text desc1)
                            (view-description:filter-text desc2)))
           (or (and (not (view-description:sort-column desc1))
                    (not (view-description:sort-column desc2)))
               (string-ci=? (view-description:sort-column desc1)
                            (view-description:sort-column desc2)))
           (eqv? (view-description:sort-order desc1)
                 (view-description:sort-order desc2)))))

(define (%make-row-comparator tbl sort-column sort-order)
  (let* ((col-index (table:column-index tbl sort-column))
         (numeric-column? (table:numeric-column? tbl sort-column))
         (accessor (if numeric-column?
                       (lambda (row)(row:element-for-numeric-sort row col-index))
                       (lambda (row)(row:element-for-string-sort row col-index))))
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

(define (view:create tbl #!key (description (view:default-description)))
  (let* ((include-deleted (view-description:include-deleted? description))
         (sort-column (view-description:sort-column description))
         (sort-order (view-description:sort-order description))
         (filter-text (view-description:filter-text description))
         (view-columns (if include-deleted
                           (%table-select-columns tbl (always #t))
                           (%table-select-columns tbl (complement column:deleted?))))
         (live-rows (if include-deleted
                        (%table-select-rows tbl (always #t))
                        (%table-select-rows tbl (complement row:deleted?))))
         (filtered-rows (if filter-text
                            (filter (lambda (row)(row:match-text? row filter-text))
                                    live-rows)
                            live-rows))
         (sorted-rows (if sort-column
                          (sort filtered-rows (%make-row-comparator tbl sort-column sort-order))
                          filtered-rows)))
    (%make-delectus-table (%make-column-sequence (list->vector view-columns))
                          (list->vector sorted-rows))))


;;; (define $cols '("Name" "Shape" "Color"))
;;; (define $rows '(("Fred" "Big" "Orange")("Barney" "Small" "Brown")("Wilma" "Slender" "White")))
;;; (define $d (table:make columns: $cols rows: $rows))
;;; (define $v1 (view:create $d description: (view:description filter-text: "w" sort-column: "Name" sort-order: $SORT_DESCENDING)))
;;; (define $v2 (view:create $d description: (view:description filter-text: "i" sort-column: "Name" sort-order: $SORT_ASCENDING)))