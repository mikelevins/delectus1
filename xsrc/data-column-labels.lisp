;;;; ***********************************************************************
;;;;
;;;; Name:          data-column-labels.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus column-labels
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; A column-label is a Delectus identity encoded as the first 26
;;; characters of a base32hex string with an "L" prepended to make it
;;; usable as a SQLite column label.

(defparameter +column-label-length+ 27)

(defmethod column-label? (thing) nil)

(defmethod column-label? ((thing string))
  (and (= (length thing) +column-label-length+)
       (char= #\L (elt thing 0))
       (let ((result t))
         (block checking
           (loop for i from 1 below (length thing)
              do (unless (find (elt thing i) binascii::*base32hex-encode-table*)
                   (setf result nil)
                   (return-from checking nil))))
         result)))

(defmethod identity->column-label ((id vector))
  (assert (identity? id)() "Not a valid identity: ~S" id)
  (concatenate 'string
               "L"
               (identity->string id)))


(defmethod identity->column-label ((id string))
  (if (identity-string? id)
      (concatenate 'string "L" id)
      (error "Not a valid identity string: ~S" id)))

(defun make-column-label ()
  (identity->column-label (makeid)))

;;; (setf $lbl (make-column-label))
;;; (column-label? $lbl)
;;; (as-keyword (make-column-label))

(defmethod column-label->identity ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label: ~S" lbl)
  (binascii:decode-base32hex lbl :start 1))

;;; (setf $lbl (make-column-label))
;;; (column-label->identity $lbl)
;;; (identity->column-label (column-label->identity $lbl))


(defmethod column-label->identity-string ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label: ~S" lbl)
  (subseq lbl 1))

;;; (setf $lbl (make-column-label))
;;; (column-label->identity-string $lbl)
