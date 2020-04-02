;;;; ***********************************************************************
;;;;
;;;; Name:          identifiers.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus list, column, and op identifiers
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Delectus 2 uses uuids as identifiers for data tables, ops, and
;;; columns, but SQLite table and columnnames cannot start with a
;;; digit, and must not contain hyphens; UUIDs contain hyphens and
;;; can start with a digit. Delectus therefore makes a simple
;;; mechanical fixup to create identifiers: it converts hyphens to
;;; underscores (which are legal in SQLite names), and prepends
;;; the string "ID".

(defmethod uuid->identifier ((id uuid:uuid))
  (concatenate 'string "ID"
               (substitute #\_ #\- (string-downcase (format nil "~A" id))
                           :test #'char=)))

(defmethod new-identifier ()
  (uuid->identifier (uuid:make-v4-uuid)))

