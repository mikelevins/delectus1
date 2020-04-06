;;;; ***********************************************************************
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       delectus 2
;;;; Purpose:       syntax extensions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)
(in-readtable :standard)

(defreadtable :sql
  (:merge :standard)
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         ` (cl:list ,@elts))))
  (:macro-char #\] (get-macro-character #\)))
  (:case :preserve))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------
