;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          presentations.scm
;;;; Project:       Delectus
;;;; Purpose:       Filtered and sorted presentations of table data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; Private
;;; ----------------------------------------------------------------------

;;; object IDs

(define %current-oid #f)
(define %next-oid #f)
(let ((current-oid 0))
  (set! %current-oid
        (lambda () current-oid))
  (set! %next-oid
        (lambda ()
          (set! current-oid (+ current-oid 1))
          (%current-oid))))

;;; object registry

(define *object-registry* (make-table))

(define (find-object oid)
  (table-ref *object-registry* oid #f))

(define (register-object! oid pres)
  (table-set! *object-registry* oid pres))


