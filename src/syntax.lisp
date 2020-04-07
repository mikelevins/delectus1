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

(defreadtable :delectus
  (:merge :standard)
  ;; list literals
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         `(cl:list ,@elts))))
  (:macro-char #\] (get-macro-character #\)))
  ;; map literals
  (:macro-char #\{ #'(lambda (stream char)
                       (declare (ignore char))
                       (let* ((elts (read-delimited-list #\} stream t)))
                         `(fset:convert 'fset:wb-map (loop for tail on (cl:list ,@elts) by #'cddr
                                                        collect (cons (first tail)
                                                                      (second tail)))))))
  (:macro-char #\} (get-macro-character #\))))

(defreadtable :sql
  (:merge :delectus)
  (:case :preserve))



