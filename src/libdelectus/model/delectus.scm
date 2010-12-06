;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus/libdelectus
;;;; Purpose:       the basic data model
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; a delectus (from the Latin "delectum", meaning something selected),
;;; is an ordered sequence of selected items. An item is a map from 
;;; keys to values.
;;;
;;; All the items in a given delectus have a shared set of keys. Those
;;; keys appear in a unique and stable order obtained by merging the
;;; keys of each item in the order that the items are added to the
;;; delectus. Some items may lack some keys before they are merged
;;; into the delectus. We handle that case by assigning the value '()
;;; to any key that an item initially lacked.

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define (%take-key-and-value plist)
  (if (null? plist)
      (values '() '() '())
      (if (null? (cdr plist))
          (error "Malformed plist: " plist)
          (values (car plist)
                  (cadr plist)
                  (cddr plist)))))

(define (%partition-plist kvs)
  (let loop ((kvs kvs)
             (ks '())
             (vs '()))
    (receive (k v tail)(%take-key-and-value kvs)
             (if (null? k)
                 (values (reverse ks) (reverse vs))
                 (loop tail (cons k ks)(cons v vs))))))

(define (add-elt vec elt)
  (vector-append vec (vector elt)))

(define (vector-position test vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (>= i len)
          #f
          (if (test (vector-ref vec i))
              i
              (loop (+ i 1)))))))

(define (filter pred ls)
  (let loop ((ls ls)
             (result '()))
    (if (null? ls)
        (reverse result)
        (let ((hd (car ls))
              (tl (cdr ls)))
          (if (pred hd)
              (loop tl (cons hd result))
              (loop tl result))))))

(define (complement fn)
  (lambda (x)
    (not (fn x))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (vector-contains? v x)
  (and (vector-position (lambda (y)(eqv? x y)) v)
       #t))


;;; ----------------------------------------------------------------------
;;; item
;;; ----------------------------------------------------------------------

(define-structure item keys vals)

(define (item-key i)
  (if (symbol? i)
      i
      (if (string? i)
          (string->symbol i)
          (error "Unknown item key type: " i))))

(define (item . kvs)
  (receive (ks vs)(%partition-plist kvs)
           (make-item (list->vector (map item-key ks)) 
                      (list->vector vs))))

(define (item.get i k)
  (let* ((k (item-key k))
         (pos (vector-position (lambda (x)(eqv? x k)) (item-keys i))))
    (if pos
        (vector-ref (item-vals i) pos)
        '())))

(define (item.set! i k v)
  (let* ((k (item-key k))
         (pos (vector-position (lambda (x)(eqv? x k)) (item-keys i))))
    (if pos
        (vector-set! (item-vals i) pos v)
        (begin
          (item-keys-set! i (add-elt (item-keys i) (item-key k)))
          (item-vals-set! i (add-elt (item-vals i) v))))
    v))

(define (item.remove! i k)
  (let* ((k (item-key k))
         (ks (item-keys i))
         (pos (vector-position (lambda (x)(eqv? x k)) ks)))
    (if pos
        (let ((klen (vector-length ks))
              (vs (item-vals i)))
          (if (< pos (- klen 1))
              (begin
                (subvector-move! ks (+ pos 1) klen ks pos)
                (subvector-move! vs (+ pos 1) klen vs pos)))
          (begin
            (vector-shrink! ks (- klen 1))
            (vector-shrink! vs (- klen 1)))))
    i))

;;; (define $it (item "Name" "Fred" "Size" "Large"))
;;; (item.get $it "Name")
;;; (item.get $it "Size")
;;; (item.get $it "Home")
;;; (item.set! $it "Home" "Bedrock")
;;; (item.remove! $it "Home")
;;; (item.remove! $it "Name")

;;; ----------------------------------------------------------------------
;;; delectus
;;; ----------------------------------------------------------------------

(define-structure delectus keys items)

(define (ensure-item-length len it)
  (let ((itlen (vector-length it)))
    (if (>= itlen len)
        it
        (vector-append it (make-vector (- len itlen) '())))))

(define (merge-keys! del ks)
  (let ((ks (filter (complement (partial vector-contains? (delectus-keys del)))
                    ks)))
    (delectus-keys-set! del (vector-append (delectus-keys del) (list->vector ks)))
    (delectus-items-set! del (list->vector (map (partial ensure-item-length (vector-length (delectus-keys del)))
                                                (vector->list (delectus-items del)))))))

(define (add-item! del it)
  (merge-keys! del (vector->list (item-keys it)))
  (delectus-items-set! del
                       (add-elt (delectus-items del)
                                (list->vector (map (lambda (k) (item.get it k))
                                                   (vector->list (delectus-keys del))))))
  del)


;;; (define $del (make-delectus (vector)(vector)))
;;; (define $it0 (item "Name" "Fred" "Size" "Large"))
;;; (add-item! $del $it0)
;;; (define $it1 (item "Name" "Barney" "Size" "Small" "Home" "Bedrock"))
;;; (add-item! $del $it1)