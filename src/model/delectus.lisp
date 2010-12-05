;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.lisp
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
;;; "delectus" is from the Latin "delectum", meaning something
;;; chosen. A delectus is an ordered set of selected items.

(defparameter $delectus-size-increment 32)

(defmethod make-delectus ((items list))
  (let* ((item-count (length items))
         (size (max $delectus-size-increment item-count))
         (result (make-array size :fill-pointer item-count :adjustable t)))
    (setf (fill-pointer result) item-count)
    (replace result items)
    result))

(defmethod add-item! ((del vector) val)
  (vector-push-extend val del 16)
  del)

(defmethod remove-item! ((del vector)(index integer))
  (assert (<= 0 index (1- (fill-pointer del)))(index) "index out of range")
  (loop
     for i from (1+ index) upto (1- (fill-pointer del))
     do (setf (elt del (1- i))
              (elt del i)))
  (decf (fill-pointer del))
  del)

;;; (setq $d (make-delectus '()))
;;; (add-item! $d "Fred")
;;; (add-item! $d "Flintstone")
;;; (add-item! $d "Bedrock")
;;; (remove-item! $d 1)

;;; ---------------------------------------------------------------------
;;; maps
;;; ---------------------------------------------------------------------
;;; a pair of delecti represents a map: one delectus contains the keys, 
;;; and the other contains the values. more generally, a collection
;;; of delecti all of the same length can be used to represent a set
;;; of maps that all share the same keys: the first delectus again
;;; contains the keys; each of the rest is an item containing a set
;;; of values for those keys

(defclass map ()
  ((keys :reader keys :initform (make-delectus nil) :initarg :keys)
   (items :reader items :initform (make-delectus nil) :initarg :items)))

(defmethod make-map ((keys list)(items list))
  (let ((key-count (length keys)))
    (assert (every (^ (i)(= key-count (length i))) items)()
            "Each item in the map must have ~S elements" key-count)
    (make-instance 'map
                   :keys (make-delectus keys)
                   :items (make-delectus (mapcar #'make-delectus items)))))

(defmethod add-item! ((map map)(elements list))
  (assert (= (length (keys map))(length elements))()
          "The new item must contain ~S elements" (length (keys map)))
  (add-item! (items map) (make-delectus elements))
  map)

#|
(setq $m
      (make-map '("Name" "Age" "Weight")
                '(("Fred" "Grown" "Heavy")
                  ("Wilma" "Mature" "Slim")
                  ("Pebbles" "Young" "Tiny"))))
(add-item! $m '("Bam Bam" "Young" "Small"))
(describe $m)
|#