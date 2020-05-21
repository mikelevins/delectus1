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
;;; A column-label is a Delectus identity string with the letter "I"
;;; prepended; the initial "I" enables us to use them as SQLite
;;; column labels without pain

(defmethod column-label? (thing) nil)

(defmethod column-label? ((thing string))
  (and (= 33 (length thing))
       (char= #\I (elt thing 0))
       (let ((result t))
         (block checking
           (loop for i from 1 below (length thing)
              do (unless (find (elt thing i) "0123456789abcdefABCDEF")
                   (setf result nil)
                   (return-from checking nil))))
         result)))

(defmethod identity->column-label ((id string))
  (assert (identity-string? id)() "Not a valid identity-string: ~S" id)
  (concatenate 'string "I" id))

(defmethod identity->column-label ((id vector))
  (assert (identity? id)() "Not a valid identity: ~S" id)
  (identity->column-label (identity->string id)))

(defun make-column-label ()
  (identity->column-label (makeid)))

;;; (setf $lbl (make-column-label))
;;; (column-label? $lbl)
;;; (as-keyword (make-column-label))

(defmethod column-label->identity-string ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label: ~S" lbl)
  (subseq lbl 1))

;;; (setf $lbl (make-column-label))
;;; (column-label->identity-string $lbl)

(defmethod column-label->identity ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label: ~S" lbl)
  (string->identity (column-label->identity-string lbl)))

;;; (setf $lbl (make-column-label))
;;; (column-label->identity $lbl)
;;; (identity->column-label (column-label->identity $lbl))
