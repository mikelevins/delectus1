;;;; ***********************************************************************
;;;;
;;;; Name:          system-syntax.lisp
;;;; Project:       delectus 2
;;;; Purpose:       syntax extensions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)


(set-syntax-from-char #\{ #\()
(set-syntax-from-char #\} #\))

(set-macro-character #\{
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\} stream t)))
                         `(convert 'wb-map (loop for tail on (cl:list ,@elts) by #'cddr
                                              collect (cons (first tail)
                                                            (second tail)))))))


(set-macro-character #\[
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         ` (cl:list ,@elts))))

(set-macro-character #\] (get-macro-character #\)))


(in-package :fset)

;;; change to a less visually-cluttered printing style for wb-maps

(defun print-wb-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "{")
    (let ((keys (fset:convert 'cl:list (fset:domain map))))
      (loop for tail on keys
         do (when tail
              (let* ((k (first tail))
                     (v (fset:@ map k)))
                (write k :stream stream)
                (write-char #\Space stream)
                (write v :stream stream)
                (when (cdr tail) (write-char #\Space stream))))))
    (format stream "}~:[~;/~:*~S~]" (map-default map))))
