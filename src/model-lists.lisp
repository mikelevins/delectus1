;;;; ***********************************************************************
;;;;
;;;; Name:          model-lists.lisp
;;;; Project:       delectus 2
;;;; Purpose:       operations on delectus list objects
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; readers
;;; ---------------------------------------------------------------------

;;; listname
;;; --------

(defun listname-op-revision (op)
  (first op))

(defun listname-op-origin (op)
  (second op))

(defun listname-op-timestamp (op)
  (third op))

(defun listname-op-name (op)
  (fourth op))

;;; comment
;;; -------

(defun comment-op-revision (op)
  (first op))

(defun comment-op-origin (op)
  (second op))

(defun comment-op-timestamp (op)
  (third op))

(defun comment-op-comment (op)
  (fourth op))

;;; columns
;;; -------

(defun columns-op-revision (op)
  (first op))

(defun columns-op-origin (op)
  (second op))

(defun columns-op-timestamp (op)
  (third op))

(defun columns-op-userdata (op)
  (mapcar 'jonathan:parse (drop 3 op)))

;;; item
;;; ----

(defun item-op-revision (op)
  (first op))

(defun item-op-origin (op)
  (second op))

(defun item-op-timestamp (op)
  (third op))

(defun item-op-itemid (op)
  (fourth op))

(defun item-op-deleted (op)
  (fifth op))

(defun item-op-userdata (op)
  (drop 5 op))

