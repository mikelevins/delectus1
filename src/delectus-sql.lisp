;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific sql constructors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; create-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-create-delectus-table ()
  (delectus::str
   "CREATE TABLE `delectus` ( "
   "`id` TEXT, `origin` TEXT, `format` TEXT, `next_revision` TEXT "
   ");"))

;;; (create-delectus-table)

;;; ---------------------------------------------------------------------
;;; populate-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-populate-delectus-table (id origin format next-revision)
  (values
   (delectus::str
    "INSERT INTO `delectus` "
    "(`id`, `origin`, `format`, `next_revision`) "
    "VALUES (?, ?, ?, ?);")
   (list id origin format next-revision)))

;;; (populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)

;;; ---------------------------------------------------------------------
;;; create-list_data-table
;;; ---------------------------------------------------------------------

(defun sql-create-list_data-table ()
  (delectus::str
   "CREATE TABLE `list_data` ( "
   "`optype` TEXT, `opid` TEXT, `origin` TEXT, "
   "`revision` INTEGER, `timestamp` TEXT, `item` TEXT, "
   "`name` TEXT, `deleted` TEXT, `peer` TEXT );"))

;;; (create-list_data-table)

;;; ---------------------------------------------------------------------
;;; add-userdata-column
;;; ---------------------------------------------------------------------

(defparameter +add-userdata-column-template+
  (delectus::str
   "ALTER TABLE `list_data` "
   "ADD `<% @var column-label %>` <% @var column-type %>;"))

(register-emb "add-userdata-column" +add-userdata-column-template+)

(defun sql-add-userdata-column (label type)
  (execute-emb "add-userdata-column"
               :env `(:column-label ,label :column-type ,type)))

;;; (add-userdata-column (delectus::makeid) "TEXT")

;;; ---------------------------------------------------------------------
;;; update-field-value
;;; ---------------------------------------------------------------------

(defparameter +update-field-value-template+
  (delectus::str
   "UPDATE `<% @var table-name %>` "
   "SET `<% @var column-label %>`=<% @var new-value %> "
   "WHERE `opid`=<% @var opid %>"))

(register-emb "update-field-value" +update-field-value-template+)

(defun sql-update-field-value (table-name column-label new-value opid)
  (execute-emb "update-field-value"
               :env `(:table-name ,table-name
                                  :column-label ,column-label
                                  :new-value ,new-value
                                  :opid ,opid)))

;;; (update-field-value "delectus" "next_revision" 3 (delectus::makeid))

;;; ---------------------------------------------------------------------
;;; assert-op
;;; ---------------------------------------------------------------------

(defun sql-assert-op (optype opid origin revision timestamp item name deleted peer)
  (values
   (execute-emb "assert-op"
                :env `(:optype ,optype
                               :opid ,opid
                               :origin ,origin
                               :revision ,revision
                               :timestamp ,timestamp
                               :item ,item
                               :name ,name
                               :deleted ,deleted
                               :peer ,peer))
   (list optype opid origin revision timestamp item name deleted peer)))


(defun sql-assert-op (optype opid origin revision timestamp item name deleted peer &key column-data)
  (let* ((column-ids (mapcar #'car column-data))
         (column-values (mapcar #'cdr column-data))
         (insert-args (append (list "optype" "opid" "origin" "revision" "timestamp" "item" "name" "deleted" "peer")
                              column-ids))
         (insert-arg-strings (mapcar (lambda (a)(format nil "`~A`" a))
                                     insert-args))
         (insert-arg-placeholders (mapcar (constantly "?") insert-args))
         (insert-args-text (delectus::join-strings ", " insert-arg-strings))
         (insert-placeholders-text (delectus::join-strings ", " insert-arg-placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      insert-args-text insert-placeholders-text)))
    (values sql
            (append (list optype opid origin revision timestamp item name deleted peer)
                    column-values))))

#|

(defparameter $opid (delectus::makeid))
(defparameter $origin delectus::*origin*)
(defparameter $item-rev 42)
(defparameter $timestamp (delectus::now-timestamp))
(defparameter $item (delectus::makeid))

(format t "~%~A~%"
(assert-op "item" $opid $origin $item-rev $timestamp $item nil nil nil
           :column-data '(("Ia189be00760e11ea8b7f930a927aa001" . 42))))

|#
