;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          item.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; item
;;; an item is an arbitrary set of attributes, represented as a map from
;;; keys to values.
;;; keys
;;; for now, Delectus item keys and values are strings (we may
;;; generalize them later to other values.) Because item keys are
;;; frequently compared for identity, and because they are required to
;;; be strings, it makes sense to intern them for speed and
;;; compactness. so we convert all keys to symbols interned in the
;;; delectus-keys package.

(defclass item ()
  ((%attrs :initform {} :initarg :attributes)))

(defmethod attributes ((it item)) 
  (with-slots (%attrs) it
    (fset:domain %attrs)))

(defmethod print-object ((it item)(s stream))
  (let ((attrs (as 'list (attributes it))))
    (print-unreadable-object (it s :type t)
      (if (> (length attrs) 3)
          (format s "~{~A ~}... " (as 'list (seq:take 3 attrs)))
          (format s "~{~A ~}" attrs)))))

(defmethod item-key ((k symbol)) k)
(defmethod item-key ((k string)) (intern k :delectus-keys))

(defmethod make-item ((attrs list))
  (make-instance 'item
                 :attributes
                 (fset:convert 'fset:wb-map
                               (loop for tl on attrs by #'cddr
                                  collect (cons (item-key (car tl))
                                                (cdr tl))))))

(defun item (&rest attrs)
  (make-item attrs))

(defun zip-item (attrs vals)
  (make-instance 'item
                 :attributes
                 (fset:convert 'fset:wb-map
                               (seq:zip (mapcar #'item-key attrs) vals))))

;;; (setq $it (zip-item '("name" "lastname")'("Fred" "Flintstone")))
;;; (describe $it)

(defun empty-item ()
  (make-item nil))

(defmethod get ((it item)(key symbol) &key default)
  (with-slots (%attrs) it
    (map:get %attrs key :default default)))

(defmethod get ((it item)(key string) &key default)
  (get it (item-key key) :default default))

(defmethod set! ((it item)(key symbol) val)
  (with-slots (%attrs) it
    (setf %attrs (fset:with %attrs key val))
    val))

(defmethod set! ((it item)(key string) val)
  (set! it (item-key key) val))

(defmethod delete! ((it item)(key symbol))
  (with-slots (%attrs) it
    (setf %attrs (fset:less %attrs key))
    val))

(defmethod delete! ((it item)(key string))
  (delete! it (item-key key)))

(defmethod equal? ((this item)(that item))
  (eql :equal (fset:compare (slot-value this '%attrs)
                            (slot-value that '%attrs))))

;;; (setq $it (item "name" "Fred"))
;;; (describe $it)
;;; (get $it "name")
;;; (set! $it "size" "Large")
;;; (attributes $it)
;;; (get $it "size")
;;; (empty-item)
;;; (equal? (empty-item)(empty-item))