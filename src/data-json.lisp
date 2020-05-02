;;;; ***********************************************************************
;;;;
;;;; Name:          data-json.lisp
;;;; Project:       delectus 2
;;;; Purpose:       converting to and from JSON
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :delectus)

(defmethod to-json ((obj wb-map))
  (let ((result nil))
    (do-map (k v obj)
      (setf result
            (cons v (cons k result))))
    (jonathan:to-json (reverse result))))

(defmethod from-json ((obj string))
  (plist->map (jonathan:parse obj)))

