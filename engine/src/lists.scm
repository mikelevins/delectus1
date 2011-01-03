;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.scm
;;;; Project:       Delectus
;;;; Purpose:       list utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (repeat n val)
  (let loop ((counter n)
             (result '()))
    (if (<= counter 0)
        (reverse result)
        (loop (- counter 1)
              (cons val result)))))

(define (copy-tree x)
  (if (list? x)
      (map copy-tree x)
      x))

(define (plist->alist pl)
  (let loop ((pl pl)
             (result '()))
    (if (null? pl)
        (reverse result)
        (if (null? (cdr pl))
            (error "Malformed plist")
            (loop (cddr pl)
                  (cons (cons (car pl)
                              (cadr pl))
                        result))))))

(define (find-associated key alist #!key (test eqv?)(default #f))
  (let loop ((entries alist))
    (if (null? entries)
        default
        (let ((entry (car entries)))
          (if (test key (car entry))
              entry
              (loop (cdr entries)))))))

(define (contains? pred ls v)
  (let loop ((ls ls))
    (if (null? ls)
        #f
        (if (pred (car ls) v)
            #t
            (loop (cdr ls))))))

(define (duplicates? pred ls)
  (let loop ((ls ls))
    (if (null? ls)
        #f
        (if (contains? pred (cdr ls) (car ls))
            #t
            (loop (cdr ls))))))