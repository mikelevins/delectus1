;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Delectus
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; simple utils
;;; ----------------------------------------------------------------------

(define (id x) x)

(define (true? x) (eqv? #t x))
(define (false? x) (eqv? #f x))

;;; ----------------------------------------------------------------------
;;; combinators
;;; ----------------------------------------------------------------------

(define (flip fn) (lambda (x y) (fn y x)))

(define (complement fn) (lambda (x) (not (fn x))))

(define (compose f1 f2)
  (lambda args
    (f1 (apply f2 args))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (conjoin f1 . fs)
  (lambda (x)
    (and (f1 x)
         (every (lambda (f) (f x))
                fs))))

(define (disjoin f1 . fs)
  (lambda (x)
    (or (f1 x)
        (any? (lambda (f) (f x))
              fs))))

(define (constantly x) (lambda args x))

(define (range x y #!optional (acc '()))
  (if (>= x y)
      (reverse acc)
      (range (+ x 1) y (cons x acc))))

;;; ----------------------------------------------------------------------
;;; sequence utils
;;; ----------------------------------------------------------------------

(define (empty? x)
  (cond
   ((string? x) (zero? (string-length x)))
   ((list? x) (zero? (length x)))
   ((pair? x) #f)
   ((vector? x) (zero? (vector-length x)))
   (#t (error "Don't know how to test whether an object of this type is empty: " x))))

;;; ----------------------------------------------------------------------
;;; list utils
;;; ----------------------------------------------------------------------

(define (list-copy ls) (map id ls))

(define (tree-copy x)
  (let recur ((x x))
    (if (not (pair? x)) x
        (cons (recur (car x)) (recur (cdr x))))))

(define (interpose item items #!optional (acc '()))
  (if (null? items)
      (reverse acc)
      (interpose
       item
       (cdr items)
       (cons (car items)
             (if (null? acc)
                 acc
                 (cons item acc))))))

(define (position-if pred ls #!optional (acc 0))
  (if (null? ls)
      #f
      (if (pred (car ls))
          acc
          (position-if pred (cdr ls) (+ acc 1)))))

(define (flatten ls)
  (let* ((result (cons '() '()))(last-elt result))
    (define (f ls)
      (cond
        ((null? ls) result)
        ((pair? (car ls)) (f (car ls)) (f (cdr ls)))
        (else (set-cdr! last-elt (cons (car ls) '()))
              (set! last-elt (cdr last-elt))
              (f (cdr ls)))))
    (f ls)
    (cdr result)))

(define (contains-duplicates? ls #!optional (test eqv?) (seen '()))
  (if (null? ls)
      #f
      (if (any? (lambda (s) (test (car ls) s))
                seen)
          #t
          (contains-duplicates? (cdr ls) 
                                test
                                (cons (car ls) seen)))))

(define (zip l1 l2) (map cons l1 l2))

(define (every? pred ls)
  (or (null? ls)
      (and (pred (car ls))
           (every? pred (cdr ls)))))

(define (any? pred ls)
  (if (null? ls)
      #f
      (or (and (pred (car ls)) (car ls))
          (any? pred (cdr ls)))))

(define (filter pred ls #!optional (acc '()))
  (if (null? ls)
      (reverse acc)
      (if (pred (car ls))
          (filter pred (cdr ls) (cons (car ls) acc))
          (filter pred (cdr ls) acc))))

(define (remove item ls #!optional (test eqv?) (acc '()))
  (if (null? ls)
      (reverse acc)
      (if (test item (car ls))
          (remove item (cdr ls) test acc)
          (remove item (cdr ls) test (cons (car ls) acc)))))

(define (distinct l #!optional (test eqv?) (acc '()))
  (if (null? l)
      (reverse acc)
      (if (position-if (lambda (x) (test (car l) x)) acc)
          (distinct (cdr l) test acc)
          (distinct (cdr l) test (cons (car l) acc)))))

;;; ----------------------------------------------------------------------
;;; alist utils
;;; ----------------------------------------------------------------------

(define (get-key alist key #!optional (default #f))
  (let ((entry (assoc key alist)))
    (or (and entry (cdr entry))
        default)))

(define (set-key! alist key val)
  (let ((entry (assoc key alist)))
    (if entry
        (begin
          (set-cdr! entry val)
          alist)
        (cons (key val)
              alist))))

;;; ----------------------------------------------------------------------
;;; string utils
;;; ----------------------------------------------------------------------

(define (str . args)
  (apply string-append
         (map (lambda (x) (if (string? x) x (object->string x)))
              args)))

(define (string-join sep . args) (apply str (interpose sep args)))

(define (string-repeat n x #!optional (acc ""))
  (if (<= n 0)
      acc
      (string-repeat (- n 1) x (string-append acc x))))

(define (empty-string? s)
  (and (string? s)
       (= 0 (string-length s))))

(define (strip-whitespace s)
  (let* ((slen (string-length s))
         (start-non-whitespace (let loop ((i 0))
                                 (if (>= i slen)
                                     #f
                                     (if (not (char-whitespace? (string-ref s i)))
                                         i
                                         (loop (+ i 1))))))
         (end-non-whitespace (let loop ((i (- slen 1)))
                               (if (< i 0)
                                   #f
                                   (if (not (char-whitespace? (string-ref s i)))
                                       i
                                       (loop (- i 1)))))))
    (if start-non-whitespace
        (substring s start-non-whitespace (+ end-non-whitespace 1))
        "")))

;;; ----------------------------------------------------------------------
;;; logging and reporting
;;; ----------------------------------------------------------------------

(define (log msg . args) (println (apply (partial format msg) args)))

(define (call-with-errors-logged thunk #!key context (banner "Error") message report-values (return-value #f))
  (let* ((context-string (if context (format " in ~s" context) ""))
         (message-string (if message (format " ~a;" message) ""))
         (values-string (if report-values (format " reported values: ~s" report-values) ""))
         (msg (format "~a~a:~a~a" banner context-string message-string values-string))
         (error-handler (lambda (err)(log msg) return-value)))
    (with-exception-catcher error-handler thunk)))