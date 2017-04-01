;;;; ***********************************************************************
;;;;
;;;; Name:          delectus2csv.scm
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
        (delectus->csv src-path))
      (display-usage)))
