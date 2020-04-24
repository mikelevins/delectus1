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
(in-readtable :standard)

(defreadtable :delectus
  (:merge :standard)
  ;; map literals
  (:macro-char #\{ #'(lambda (stream char)
                       (declare (ignore char))
                       (let* ((elts (read-delimited-list #\} stream t)))
                         `(convert 'wb-map (loop for tail on (cl:list ,@elts) by #'cddr
                                              collect (cons (first tail)
                                                            (second tail)))))))
  (:macro-char #\} (get-macro-character #\))))


(in-package :fset)

;;; change to a less visually-cluttered printing style for wb-maps

(defun print-wb-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "{")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream)
      (write-char #\Space stream)
      (write y :stream stream))
    (format stream " }~:[~;/~:*~S~]" (map-default map))))
