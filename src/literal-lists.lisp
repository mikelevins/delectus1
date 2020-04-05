;;;; ***********************************************************************
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       syntax for sequence literals
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------


(set-macro-character #\[
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         ` (cl:list ,@elts))))

(set-macro-character #\] (get-macro-character #\)))
