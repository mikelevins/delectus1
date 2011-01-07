;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.scm
;;;; Project:       Delectus
;;;; Purpose:       map values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type map:map
  id: F956C5ED-8EF2-4C35-8E37-794EDF4381D0
  constructor: %make-map
  (entries map:entries map:set-entries!))

(define (map:make entries)
  (%make-map (copy-tree entries)))

(define (map:map . entries)
  (map:make (plist->alist entries)))

(define (map:keys m)
  (map car (map:entries m)))

(define (map:vals m)
  (map cdr (map:entries m)))

(define (map:get m k #!key (default #f)(test eqv?))
  (let ((entry (find-associated k (map:entries m) test: test default: default)))
    (if (eqv? default entry)
        default
        (cdr entry))))

(define (map:put! m k v #!key (default #f)(test eqv?))
  (let ((entry (find-associated k (map:entries m) test: test default: default)))
    (if (eqv? default entry)
        (begin
          (map:set-entries! m
                            (cons (cons k v)
                                  (map:entries m)))
          v)
        (begin
          (set-cdr! entry v)
          v))))

