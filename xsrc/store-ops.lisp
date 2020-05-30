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

;;; ---------------------------------------------------------------------
;;; op input validation
;;; ---------------------------------------------------------------------

(defun db-ensure-origin-string (db thing)
  (cond
    ((null thing)
     (make-origin-string (process-identity)
                         (sqlite::database-path db)))
    ((origin-string? thing) thing)
    (t (error "Invalid origin in ~S; expected an origin-string or nil."
              thing))))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path)(sqlite::database-path db))

(defun db-ensure-revision-number (db thing)
  (cond
    ((null thing)(db-get-next-revision db))
    ((and (integerp thing)
          (>= thing 0))
     thing)
    (t (error "Invalid revision number in ~S; expected a nonnegative integer or nil."
              thing))))

(defun db-ensure-item-order-number (db thing)
  (cond
    ((null thing)(db-get-next-item-order db))
    ((typep thing 'double-float) thing)
    (t (error "Invalid item-order number in ~S; expected a double-float or nil."
              thing))))

(defun db-ensure-timestamp (db thing)
  (cond
    ((null thing)(delectus-timestamp-now))
    ((and (integerp thing)
          (>= thing 0))
     thing)
    (t (error "Invalid timestamp in ~S; expected a nonnegative integer or nil."
              thing))))

(defun db-ensure-listname-string (db thing)
  (cond
    ((stringp thing) thing)
    (t (error "Invalid listname in ~S; expected a string."
              thing))))

(defun db-ensure-comment-string (db thing)
  (cond
    ((stringp thing) thing)
    (t (error "Invalid comment text in ~S; expected a string."
              thing))))

(defun db-ensure-columns-data (db thing)
  (cond
    ((stringp thing) thing)
    (t (error "Invalid columns data in ~S; expected a JSON string."
              thing))))

;;; ---------------------------------------------------------------------
;;; listname op
;;; ---------------------------------------------------------------------

(defmethod db-insert-listname-op ((db sqlite-handle)
                                  &key
                                    origin
                                    revision
                                    item-order
                                    timestamp
                                    listname)
  (bind ((origin (db-ensure-origin-string db origin))
         (revision (db-ensure-revision-number db revision))
         (item-order (db-ensure-item-order-number db item-order))
         (timestamp (db-ensure-timestamp db timestamp))
         (listname (db-ensure-listname-string db listname))
         (name-json (jonathan:to-json listname))
         (sql vals (sqlgen-insert-listname-op origin revision item-order timestamp name-json)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; comment op
;;; ---------------------------------------------------------------------

(defmethod db-insert-comment-op ((db sqlite-handle)
                                 &key
                                   origin
                                   revision
                                   item-order
                                   timestamp
                                   comment)
  (bind ((origin (db-ensure-origin-string db origin))
         (revision (db-ensure-revision-number db revision))
         (item-order (db-ensure-item-order-number db item-order))
         (timestamp (db-ensure-timestamp db timestamp))
         (comment (db-ensure-comment-string db comment))
         (comment-json (jonathan:to-json comment))
         (sql vals (sqlgen-insert-comment-op origin revision item-order timestamp comment-json)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; columns op
;;; ---------------------------------------------------------------------

(defmethod db-insert-columns-op ((db sqlite-handle)
                                 &key
                                   origin
                                   revision
                                   item-order
                                   timestamp
                                   columns)
  (bind ((origin (db-ensure-origin-string db origin))
         (revision (db-ensure-revision-number db revision))
         (item-order (db-ensure-item-order-number db item-order))
         (timestamp (db-ensure-timestamp db timestamp))
         (columns-data (db-ensure-columns-data db columns))
         (sql vals (sqlgen-insert-columns-op origin revision item-order timestamp columns-data)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; item op
;;; ---------------------------------------------------------------------

