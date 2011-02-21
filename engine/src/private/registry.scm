;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          registry.scm
;;;; Project:       Delectus
;;;; Purpose:       obtaining views of delectus data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type registry-entry
  id: 325DABB4-1D51-4B3D-B083-2F3BC0408E99
  constructor: %make-registry-entry
  (table registry-entry:table registry-entry:set-table!)
  (id registry-entry:id registry-entry:set-id!)
  (view registry-entry:view registry-entry:set-view!)
  (view-description registry-entry:view-description registry-entry:set-view-description!))

(define (registry-entry:make #!key (table #f)(view #f)(view-description #f))
  (%make-registry-entry table view view-description))

(define $id->entry-table (make-table test: eqv?))
(define $view->id-table (make-table test: eq?))

(define reg:current-id #f)
(define reg:next-id #f)
(let ((current-delectus-id 0))
  (set! reg:current-id (lambda () current-delectus-id))
  (set! reg:next-id
        (lambda ()
          (set! current-delectus-id (+ current-delectus-id 1))
          (reg:current-id))))

(define (reg:register-delectus! tbl)
  (if (reg:view->base-view view)
      (error "Attempted to register a view as a base table" tbl)
      (let ((already-id (reg:view->id id)))
        (if already-id
            already-id
            (let* ((new-id (reg:next-id))
                   (regentry (registry-entry:make table: tbl id: new-id)))
              (table-set! $view->id-table view new-id)
              (table-set! $id->entry-table new-id regentry)
              new-id)))))

(define (reg:update-description! regentry desc)
  (if (view:null-description? desc)
      desc
      (if (view:description-equal? desc (registry-entry:view-description regentry))
          desc
          (begin
            (registry-entry:set-view-description! regentry desc)
            (registry-entry:set-view! regentry (view:create (registry-entry:table regentry)
                                                            description: desc))))))

(define (reg:id->view id #!key (description #f))
  (let ((entry (table-ref $id->entry-table id #f)))
    (if entry
        (if (view:null-description? description)
            (registry-entry:table entry)
            (begin
              (reg:update-description! entry description)
              (registry-entry:view entry)))
        (error "No such ID registered" id))))

(define (reg:view->id view)
  (table-ref $view->id-table view #f))

(define (reg:view->base-view view)
  (table-ref $view->base-table view #f))

(define (reg:id->base-table id)
  (let ((entry (table-ref $id->entry-table id #f)))
    (if entry
        (registry-entry:table entry)
        (error "No such ID registered" id))))

(define (reg:invalidate-view! id)
  (let ((entry (table-ref $id->entry-table id #f)))
    (if entry
        (registry-entry:set-view! entry #f)
        (error "No such ID registered" id))))

