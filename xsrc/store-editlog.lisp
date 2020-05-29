;;;; ***********************************************************************
;;;;
;;;; Name:          store-editlog.lisp
;;;; Project:       delectus 2
;;;; Purpose:       operations on the editlog
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defmethod db-insert-listname ((db sqlite-handle) &key origin revision order timestamp name)
  ;; normally, revision, order, and timestamp should be nil, and we
  ;; compute them. they should be non-nil only if we are
  ;; reconstructing a previously-existing op, such as when copying ops
  ;; from one copy of a list to another.
  (assert (origin? origin)() "Not a valid origin: ~S" origin)
  (let* ((target "listname")
         (revision (or revision (db-get-next-revision db target origin)))
         (order (or order (db-get-next-order db)))
         (timestamp (or timestamp (delectus-timestamp-now))))
    ))

