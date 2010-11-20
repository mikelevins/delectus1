;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       sorting utilities
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; model is the in-RAM data model for delectus. A Delectus document is
;;; conceptually a list whose elements are lists of fields. A field is
;;; a container for either a number or a string.

;;; ---------------------------------------------------------------------
;;; values
;;; ---------------------------------------------------------------------
;;; all Delectus values are either strings or numbers. Any string that
;;; can be parsed by Common Lisp as a decimal number is converted to
;;; the equivalent number for sorting

(defmethod value (v)
  (declare (ignore v))
  (error "Unrecognized value: ~S" v))

(defmethod value ((v null)) "")

(defmethod value ((v number)) v)

(defmethod value ((v string))
  (handler-case (parse-number:parse-number v)
    (condition (err) v)))

;;; (value nil)
;;; (value 'foo)
;;; (value "foo")
;;; (value "12.345")

