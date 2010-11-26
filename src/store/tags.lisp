;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tags.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       type tags for binary storage of objects
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------
(defparameter $delectus-format-alpha-1 0)
(defparameter $delectus-format-alpha-2 1)
(defparameter $delectus-format-alpha-4 2)
(defparameter $delectus-format-beta-2 3)
(defparameter $delectus-format-1.9a 4)

(defun current-store-format-version () $delectus-format-1.9a)

(defparameter $delectus-format-sentinel
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(68 69 76 67)))

;;; ---------------------------------------------------------------------
;;; serialization tags
;;; ---------------------------------------------------------------------

(defparameter $tag-bool 0)
(defparameter $tag-uint32 1)
(defparameter $tag-string 2)
(defparameter $tag-symbol 3)
(defparameter $tag-vector 4)


