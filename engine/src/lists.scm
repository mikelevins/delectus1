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

(define (range m n #!key (by 1))
  (let ((step-fn (if (<= m n) + -))
        (test-fn (if (<= m n) >= <=)))
    (let loop ((k m)
               (result '()))
      (if (test-fn k n)
          (reverse result)
          (loop (step-fn k by)(cons k result))))))

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

(define (filter pred ls)
  (let loop ((ls ls)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (pred (car ls))
            (loop (cdr ls) (cons (car ls) result))
            (loop (cdr ls) result)))))

(define (remove item ls #!key (test eqv?))
  (let loop ((ls ls)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (test item (car ls))
            (loop (cdr ls) result)
            (loop (cdr ls) (cons (car ls) result))))))

(define (select indexes ls)
  (let loop ((ls ls)
             (j 0)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (contains? = indexes j)
            (loop (cdr ls)(+ j 1)(cons (car ls) result))
            (loop (cdr ls)(+ j 1) result)))))