;;;; ***********************************************************************
;;;;
;;;; Name:          delectus2lisp.scm
;;;; Project:       Delectus 1->2 conversion utility
;;;; Purpose:       application startup
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(let ((args (cdr (command-line))))
  ;; we require 1 inputs: INPATH
  (if (= 1 (length args))
      (let* ((src-path (car args)))
        ;; dooo eet!
        (write (delectus->lisp src-path)))
      (display-usage)))
