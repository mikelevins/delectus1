;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific sql constructors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sql)

;;; ---------------------------------------------------------------------
;;; create-delectus-table
;;; ---------------------------------------------------------------------

(defparameter +create-delectus-table-template+
  "
CREATE TABLE delectus (
  id TEXT,
  origin TEXT,
  format TEXT,
  next_revision TEXT
);
")


(register-emb "create-delectus-table" +create-delectus-table-template+)

(defun create-delectus-table ()
  (execute-emb "create-delectus-table"))

;;; (create-delectus-table)

;;; ---------------------------------------------------------------------
;;; populate-delectus-table
;;; ---------------------------------------------------------------------

(defparameter +populate-delectus-table-template+
  "INSERT INTO delectus (id, origin, format, next_revision) VALUES (?, ?, ?, ?);")

(register-emb "populate-delectus-table" +populate-delectus-table-template+)

(defun populate-delectus-table (id origin format next-revision)
  (values
   (execute-emb "populate-delectus-table"
                :env `(:id ,id :origin ,origin :format ,format :next-revision ,next-revision))
   (list id origin format next-revision)))

;;; (populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)

;;; ---------------------------------------------------------------------
;;; create-list_data-table
;;; ---------------------------------------------------------------------

(defparameter +create-list_data-table-template+
  "
CREATE TABLE list_data (
  optype TEXT,
  opid TEXT,
  origin TEXT,
  revision INTEGER,
  timestamp TEXT,
  item TEXT,
  name TEXT,
  deleted TEXT,
  peer TEXT
);
")


(register-emb "create-list_data-table" +create-list_data-table-template+)

(defun create-list_data-table ()
  (execute-emb "create-list_data-table"))

;;; (create-list_data-table)

;;; ---------------------------------------------------------------------
;;; add-userdata-column
;;; ---------------------------------------------------------------------

(defparameter +add-userdata-column-template+
  "ALTER TABLE list_data ADD <% @var column-label %> <% @var column-type %>;")

(register-emb "add-userdata-column" +add-userdata-column-template+)

(defun add-userdata-column (label type)
  (execute-emb "add-userdata-column"
               :env `(:column-label ,label :column-type ,type)))

;;; (add-userdata-column (delectus::makeid) "TEXT")

;;; ---------------------------------------------------------------------
;;; update-field-value
;;; ---------------------------------------------------------------------

(defparameter +update-field-value-template+
  "
UPDATE <% @var table-name %> SET <% @var column-label %>=<% @var new-value %>
WHERE opid=<% @var opid %>
")

(register-emb "update-field-value" +update-field-value-template+)

(defun update-field-value (table-name column-label new-value opid)
  (execute-emb "update-field-value"
               :env `(:table-name ,table-name
                                  :column-label ,column-label
                                  :new-value ,new-value
                                  :opid ,opid)))

;;; (update-field-value "delectus" "next_revision" 3 (delectus::makeid))


;;; ---------------------------------------------------------------------
;;; assert-op
;;; ---------------------------------------------------------------------

(defparameter +assert-op-template+
  "
INSERT INTO list_data (optype, opid, origin, revision, timestamp, item, name, deleted, peer) 
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);
")

(register-emb "assert-op" +assert-op-template+)

(defun assert-op (optype opid origin revision timestamp item name deleted peer)
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

;;; (defparameter $opid (delectus::makeid))
;;; (defparameter $origin delectus::*origin*)
;;; (defparameter $timestamp (delectus::now-timestamp))
;;; (defparameter $item (delectus::makeid))
;;; (assert-op "item" $opid $origin 3 $timestamp $item nil nil nil)



