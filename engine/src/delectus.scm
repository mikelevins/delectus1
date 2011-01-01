;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       data structures and operations
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; constants
;;; ----------------------------------------------------------------------

(define $delectus-format-alpha-1 0)
(define $delectus-format-alpha-2 1)
(define $delectus-format-alpha-4 2)
(define $delectus-format-beta-2 3)
(define $delectus-format-1.0 4)

(define (current-delectus-format-version) $delectus-format-1.0)

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define (ensure-list thing)
  (if (list? thing)
      thing
      (if (vector? thing)
          (vector->list thing)
          (error "Can't convert value to a list" thing))))

(define (default-value) '())

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define-type column
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-column
  column-label deleted?)

(define (make-column label #!key deleted?)
  (%make-column label deleted?))

(define (ensure-column thing)
  (if (column? thing)
      thing
      (if (string? thing)
          (make-column thing)
          (error "Can't convert value to a column" thing))))

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------

(define-type row
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-row
  row-elements deleted?)

(define (make-row elements #!key deleted?)
  (let* ((elt-count (length elements))
         (elts (make-vector elt-count)))
    (let loop ((i 0)
               (es elements))
      (if (null? es)
          (%make-row elts deleted?)
          (begin
            (vector-set! elts i (car es))
            (loop (+ i 1)(cdr es)))))))

(define (ensure-row thing)
  (if (row? thing)
      thing
      (if (list? thing)
          (make-row thing)
          (if (vector? thing)
              (make-row (vector->list thing))
              (error "Can't convert value to a row" thing)))))

;;; ----------------------------------------------------------------------
;;; delecti
;;; ----------------------------------------------------------------------

(define-type delectus
  id: 2D65FC83-52F9-4A24-BF8E-99A8CA106583
  constructor: %make-delectus
  delectus-columns delectus-rows)

(define (make-delectus columns rows)
  (let* ((cols (map ensure-column (ensure-list columns)))
         (rows (map ensure-row (ensure-list (map ensure-list rows)))))
    (%make-delectus (list->vector cols)
                    (list->vector rows))))

;;; (make-delectus '("Name" "Color") '(("Fred" "orange")("Wilma" "White")("Barney" "Brown")))

;;; ----------------------------------------------------------------------
;;; delectus-views
;;; ----------------------------------------------------------------------
;;; views provide presentation API such as sorting, filtering, and hiding
;;; deleted items. the public API always operates on a delectus-view

(define-type delectus-view
  id: D604C012-3A9D-4D7B-BE70-4ACB9260659F
  constructor: %make-delectus-view
  delectus
  show-deleted?
  sort-column sort-order sort-type
  filter-text
  changed?)

;;; ----------------------------------------------------------------------
;;; registry
;;; ----------------------------------------------------------------------
;;; each delectus-view created is assigned a per-session id
;;; number. the registry keeps track of these id numbers, and enables
;;; the client API to specify operations on a particular delectus by
;;; id number.

(define *api-id->delectus-view-table* (make-table))
(define *delectus-view->api-id-table* (make-table))

(define next-delectus-id #f)
(let ((id 0))
  (set! next-delectus-id
        (lambda ()
          (set! id (+ id 1))
          id)))

(define (register-delectus-view dview)
  (let ((found-id (table-ref *delectus-view->api-id-table* dview #f)))
    (if found-id
        found-id
        (let ((next-id (next-delectus-id)))
          (table-set! *delectus-view->api-id-table* dview next-id)
          (table-set! *api-id->delectus-view-table* next-id dview)
          next-id))))

(define (new-delectus-view)
  (register-delectus-view 
   (%make-delectus-view (make-delectus '() '())
                        #f
                        #f #f #f
                        #f
                        #f)))

(define (find-delectus id)
  (table-ref *api-id->delectus-view-table* id #f))