;;;; ***********************************************************************
;;;;
;;;; Name:          store-ops.lisp
;;;; Project:       delectus 2
;;;; Purpose:       inserting and fetching ops
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defmethod db-insert-listname ((db sqlite-handle) &key origin revision order timestamp name)
  (assert (origin-string? origin)() "Invalid origin string in ~S" origin)
  (assert (integerp revision)() "Invalid revision in ~S" revision)
  (assert (typep order 'double-float)() "Invalid order in ~S" order)
  (assert (stringp name)() "Invalid list name in ~S" name)
  (bind ((timestamp (or timestamp (delectus-timestamp-now)))
         (sql vals (sqlgen-insert-listname origin revision order timestamp name)))
    (apply 'execute-non-query db sql vals)))
