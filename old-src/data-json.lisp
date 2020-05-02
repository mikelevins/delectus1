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
  (let* ((plist (wb-map->plist obj)))
    (jonathan:to-json plist)))

;;; (to-json {:a 1 :b 2})

(defmethod from-json ((obj string))
  (plist->map (jonathan:parse obj)))
