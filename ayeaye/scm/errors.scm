;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          errors.scm
;;;; Project:       Delectus
;;;; Purpose:       error-handling for Delectus
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; In the deployed Delectus application, the engine is driven by a 
;;; Cocoa UI through C functions. When an error occurs in the back end,
;;; it needs to do two things: (1) post a notification that permits
;;; the Cocoa UI to display an informative error message, and (2) return
;;; a reasonable value to the C function that called the offending
;;; Scheme code. Where a pure Scheme application might use error, or
;;; Gambit's exceptions, we instead provide a FIFO for error notifications.

;;; The scheme works like this: when the Delectus engine encounters an
;;; error, it constructs a description of the error and posts it
;;; to the global error FIFO, and then returns an error value. Each
;;; API function is defined to return a specific type of argument
;;; suitable for use with the C code that calls into the API. For each
;;; such type, an error value is also defined. In case of an error in the
;;; Scheme engine, the C code will see an error value, and will also
;;; receive a notification that an item has been added to the error
;;; FIFO.

;;; On the UI side, whenever an error notification is received, and
;;; the error UI is not already onscreen, then the error UI is
;;; presented with the latest error displayed. All error reports in the
;;; FIFO are accessible from the error UI. Displaying an error in the
;;; error UI removes it from the FIFO.

(define $print-errors? #f)

(define next-error-id
  (let ((current-id 0))
    (lambda () 
      (let ((curr current-id))
        (set! current-id (+ current-id 1))
        curr))))

(define-structure error-report id context error-object message)

(define (make-error #!key context error-object message)
  (make-error-report (next-error-id) context error-object message))

;;; the queue is an alist. The newest error is at the front; the oldest is at the end
(define $error-queue '())

(define (clear-all-errors)(set! $error-queue '()))

(define (errcount)(length $error-queue))

(define (report-error #!key (context "Unknown") (error #f) (message "Error"))
  (let ((e (make-error context: (format "~s"context) error-object: error message: message)))
    (set! $error-queue (cons (cons (error-report-id e) e)
                             $error-queue))
    (if $print-errors?
        (display (str
                  (format "~s: context: ~s~%  error-object: ~s~%"
                          (error-report-message e)
                          (error-report-context e)
                          (error-report-error-object e)))))))

(define (next-error)
  (if (empty? $error-queue)
      #f
      (cdr (list-ref $error-queue (- (length $error-queue) 1)))))

(define (get-nth-error index)
  (if (>= index (length $error-queue))
      #f
      (cdr (list-ref (reverse $error-queue) index))))

(define (remove-error eid)
  (set! $error-queue 
        (filter (lambda (x) (not (= (error-report-id (cdr x)) eid)))
                $error-queue)))

(define (id->error eid)
  (if (empty? $error-queue)
      #f
      (let ((entry (assv eid $error-queue)))
        (if entry
            (cdr entry)
            #f))))

(define (reporting-errors thunk #!key (context #f) (message #f) (default #f))
  (let ((error-handler (lambda (err) 
                         (report-error context: context error: err message: message)
                         default)))
    (with-exception-catcher error-handler thunk)))