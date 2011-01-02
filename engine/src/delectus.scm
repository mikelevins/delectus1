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

(define (nothing) $NO-VALUE)
(define (nothing? x) (zero? x))
(define (something? x) (not (nothing? x)))

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define-type column
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-column
  label deleted?)

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
  elements deleted?)

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

(define (row-add-last! row val)
  (row-elements-set! row (vector-add-last (row-elements row) val)))

;;; ----------------------------------------------------------------------
;;; delecti
;;; ----------------------------------------------------------------------

(define-type delectus
  id: 2D65FC83-52F9-4A24-BF8E-99A8CA106583
  constructor: %make-delectus
  uuid columns rows)

(define (make-delectus columns rows #!key (uuid (make-uuid)))
  (let* ((cols (map ensure-column (ensure-list columns)))
         (rows (map ensure-row (ensure-list (map ensure-list rows)))))
    (%make-delectus uuid
                    (list->vector cols)
                    (list->vector rows))))

(define (count-columns del)
  (vector-length (delectus-columns del)))

(define (count-rows del)
  (vector-length (delectus-rows del)))

(define (column-index del label)
  (vector-position (delectus-columns del) label (lambda (c l) (string-ci=? (column-label c) l))))


;;; (make-delectus '("Name" "Color") '(("Fred" "orange")("Wilma" "White")("Barney" "Brown")))

;;; ----------------------------------------------------------------------
;;; views
;;; ----------------------------------------------------------------------
;;; views provide presentation API such as sorting, filtering, and hiding
;;; deleted items. the public API always operates on a view

(define-type view
  id: D604C012-3A9D-4D7B-BE70-4ACB9260659F
  constructor: %make-view
  delectus
  show-deleted?
  sort-column sort-order sort-type
  filter-text
  changed?)

;;; TODO: implement sorting and filtering
(define (view-rows v)
  (delectus-rows (view-delectus v)))

;;; ----------------------------------------------------------------------
;;; registry
;;; ----------------------------------------------------------------------
;;; each view created is assigned a per-session id number. the
;;; registry keeps track of these id numbers, and enables the client
;;; API to specify operations on a particular delectus by id number.
;;; API id numbers do not persist across sessions, and should not be
;;; used to identify a delectus across sessions.

(define *api-id->view-table* (make-table))
(define *view->api-id-table* (make-table))

(define next-delectus-id #f)
(let ((id 0))
  (set! next-delectus-id
        (lambda ()
          (set! id (+ id 1))
          id)))

(define (register-view dview)
  (let ((found-id (table-ref *view->api-id-table* dview #f)))
    (if found-id
        found-id
        (let ((next-id (next-delectus-id)))
          (table-set! *view->api-id-table* dview next-id)
          (table-set! *api-id->view-table* next-id dview)
          next-id))))

;;; ----------------------------------------------------------------------
;;; API support
;;; ----------------------------------------------------------------------

(define (find-view id)
  (table-ref *api-id->view-table* id #f))

