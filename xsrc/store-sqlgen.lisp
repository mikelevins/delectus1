;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       generating SQL for Delectus store operations
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; sqlgen::get-next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen::get-next-revision ()
  (values
   (delectus::trim "

SELECT `next_revision` FROM `delectus`

")
   nil))

;;; (sqlgen::get-next-revision)

;;; ---------------------------------------------------------------------
;;; sqlgen::increment-next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen::increment-next-revision ()
  (values
   (delectus::trim "

UPDATE `delectus` SET `next_revision` = `next_revision` + 1

")
   nil))

;;; (sqlgen::increment-next-revision)


;;; ---------------------------------------------------------------------
;;; sqlgen::get-next-iref
;;; ---------------------------------------------------------------------

(defun sqlgen::get-next-iref ()
  (values
   (delectus::trim "

SELECT `next_iref` FROM `delectus`

")
   nil))

;;; (sqlgen::get-next-iref)

;;; ---------------------------------------------------------------------
;;; sqlgen::increment-next-iref
;;; ---------------------------------------------------------------------

(defun sqlgen::increment-next-iref ()
  (values
   (delectus::trim "

UPDATE `delectus` SET `next_iref` = `next_iref` + 1

")
   nil))

;;; (sqlgen::increment-next-iref)


;;; ---------------------------------------------------------------------
;;; sqlgen::create-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen::create-delectus-table ()
  (values
   (delectus::trim "

CREATE TABLE `delectus` ( 
  `id` TEXT, 
  `origin` TEXT, 
  `format` TEXT, 
  `next_revision` INTEGER,
  `next_iref` INTEGER )

")
   nil))

;;; (sqlgen::create-delectus-table)


;;; ---------------------------------------------------------------------
;;; sqlgen::init-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen::init-delectus-table (list-id list-origin format-version revision iref)
  (values
   (delectus::trim "

INSERT INTO `delectus` (`id`, `origin`, `format`, `next_revision`, `next_iref`) VALUES (?, ?, ?, ?, ?) 

")
   (list list-id list-origin format-version revision iref)))

;;; (sqlgen::init-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 0 0)


;;; ---------------------------------------------------------------------
;;; sqlgen::create-identities-table
;;; ---------------------------------------------------------------------

(defun sqlgen::create-identities-table ()
  (values
   (delectus::trim "

CREATE TABLE `identities` ( 
  `iref` INTEGER, 
  `identity` TEXT )

")
   nil))

;;; (sqlgen::create-identities-table)

;;; ---------------------------------------------------------------------
;;; sqlgen::insert-identity
;;; ---------------------------------------------------------------------

(defun sqlgen::insert-identity (iref identity)
  (values
   (delectus::trim "

INSERT INTO `identities` (`iref`, `identity`) VALUES (?, ?) 

")
   (list iref identity)))

;;; (sqlgen::insert-identity 0 delectus::*origin*)

;;; ---------------------------------------------------------------------
;;; sqlgen::create-listdata-table
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sqlgen::create-item-revision-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen::create-item-revision-origin-index ()
  (values
   (delectus::trim "

CREATE INDEX `idx_item_revision_origin` 
ON `list_data` (`item`, `revision`, `origin`)

")
   nil))

;;; (sqlgen::create-item-revision-origin-index)
