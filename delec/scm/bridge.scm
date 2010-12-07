;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bridge.scm
;;;; Project:       Delectus
;;;; Purpose:       A cache for Objective-C objects that must be allocated
;;;;                by Scheme code
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; The Objective-C UI code frequently queries the Scheme back-end to 
;;; obtain data such as column labels and row contents. The Objective-C
;;; code naturally expects to receive Objective-C values, and the Scheme 
;;; code naturally allocates and returns them. Many of these values
;;; are NSStrings and other dynamically-allocated objects, and many of 
;;; of them get used over and over. Rather than repeatedly allocate and 
;;; deallocate many essentially identical objects, it makes sense to
;;; cache them and return the same allocated object when the returned
;;; value is the same.


(c-declare "#include <Cocoa/Cocoa.h>")
(c-declare "#include <AppKit/AppKit.h>")

(c-define (c:empty-assoc) () 
          scheme-object 
          "empty_association" ""
          '())

(c-define (c:add-assoc-float al k f) (scheme-object char-string float) 
          scheme-object 
          "add_assoc__float" ""
          (let ((entry (assoc k al)))
            (if entry
                (begin
                  (set-cdr! entry f)
                  al)
                (cons (cons k f)
                      al))))

(c-define (c:add-assoc-string al k s) (scheme-object char-string char-string) 
          scheme-object 
          "add_assoc_string" ""
          (let ((entry (assoc k al)))
            (if entry
                (begin
                  (set-cdr! entry s)
                  al)
                (cons (cons k s)
                      al))))

(define bridge:$ns-string-table (make-table test: string=?))

(define (bridge:string->ns-string str)
  (call-with-errors-logged
   (lambda ()
     (let ((s (table-ref bridge:$ns-string-table str #f)))
       (or s
           (begin
             (set! s (cocoa:make-ns-string str))
             (table-set! bridge:$ns-string-table str s)
             s))))
   context: (format "(bridge:string->ns-string \"~a\")" str)
   message: "Error getting an NSString"
   report-values: `((str ,str))
   return-value: #f))

(define (bridge:string-list->nsstring-array labels)
  (let ((ns-strings (map bridge:string->ns-string labels))
        (arr (cocoa:make-ns-mutable-array)))
    (let loop ((ns-strs ns-strings))
      (if (null? ns-strs)
          arr
          (begin
            (cocoa:add-ns-string arr (car ns-strs))
            (loop (cdr ns-strs)))))))