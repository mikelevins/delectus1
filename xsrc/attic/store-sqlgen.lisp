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
;;; sqlgen-get-next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-revision ()
  (values
   (delectus::trim
    (format nil "

SELECT `next_revision` FROM `~A`

" *delectus-table-name*))
   nil))

;;; (sqlgen-get-next-revision)

;;; ---------------------------------------------------------------------
;;; sqlgen-increment-next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen-increment-next-revision ()
  (values
   (delectus::trim
    (format nil "

UPDATE `~A` SET `next_revision` = `next_revision` + 1

" *delectus-table-name*))
   nil))

;;; (sqlgen-increment-next-revision)


;;; ---------------------------------------------------------------------
;;; sqlgen-get-next-iref
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-iref ()
  (values
   (delectus::trim
    (format nil "

SELECT `next_iref` FROM `~A`

" *delectus-table-name*))
   nil))

;;; (sqlgen-get-next-iref)

;;; ---------------------------------------------------------------------
;;; sqlgen-increment-next-iref
;;; ---------------------------------------------------------------------

(defun sqlgen-increment-next-iref ()
  (values
   (delectus::trim
    (format nil "

UPDATE `~A` SET `next_iref` = `next_iref` + 1

" *delectus-table-name*))
   nil))

;;; (sqlgen-increment-next-iref)


;;; ---------------------------------------------------------------------
;;; sqlgen-create-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-delectus-table ()
  (values
   (delectus::trim
    (format nil "

CREATE TABLE `~A` ( 
  `id` TEXT, 
  `origin` TEXT, 
  `format` TEXT, 
  `next_revision` INTEGER,
  `next_iref` INTEGER )

" *delectus-table-name*))
   nil))

;;; (sqlgen-create-delectus-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-init-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen-init-delectus-table (list-id list-origin format-version revision iref)
  (values
   (delectus::trim
    (format nil "

INSERT INTO `~A` (`id`, `origin`, `format`, `next_revision`, `next_iref`) VALUES (?, ?, ?, ?, ?) 

" *delectus-table-name*))
   (list list-id list-origin format-version revision iref)))

;;; (sqlgen-init-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 0 0)


;;; ---------------------------------------------------------------------
;;; sqlgen-create-identities-table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-identities-table ()
  (values
   (delectus::trim
    (format nil "

CREATE TABLE `~A` ( 
  `iref` INTEGER, 
  `identity` TEXT )

" *identities-table-name*))
   nil))

;;; (sqlgen-create-identities-table)

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-identity
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-identity (iref identity)
  (values
   (delectus::trim
    (format nil "

INSERT INTO `~A` (`iref`, `identity`) VALUES (?, ?) 

" *identities-table-name*))
   (list iref identity)))

;;; (sqlgen-insert-identity 0 delectus::*origin*)

;;; ---------------------------------------------------------------------
;;; sqlgen-create-listdata-table
;;; ---------------------------------------------------------------------
;;; type: "listname", "sync", "columns", or "item"
;;; opid: an iref that points to the identity of the op
;;; origin: an iref that points to the identity of the origin node
;;; timestamp: an ISO-8601 string that gives the time the op was
;;;            created, according to its origin node
;;; name: [TEXT, used by "listname"] the name of the list
;;; item: [INTEGER, used by "item"] a reference to the item's identity
;;; deleted: [INTEGER, used by "item"] whether this item is marked deleted
;;; peer: [INTEGER, used by "sync"] a reference to the identity of the
;;; node we synced with

(defun sqlgen-create-listdata-table ()
  (values
   (delectus::trim
    (format nil "

CREATE TABLE `~A` ( 
  `type` TEXT, 
  `opid` INTEGER, 
  `origin` INTEGER, 
  `timestamp` TEXT, 
  `name` TEXT, 
  `item` INTEGER, 
  `deleted` INTEGER, 
  `peer` INTEGER )

" *listdata-table-name*))
   nil))

;;; (sqlgen-create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-create-item-revision-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen-create-item-revision-origin-index ()
  (values
   (delectus::trim
    (format nil "

CREATE INDEX `~A` 
ON `~A` (`item`, `revision`, `origin`)

" *item-revision-origin-index-name* *listdata-table-name*))
   nil))

;;; (sqlgen-create-item-revision-origin-index)

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-listname-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname-op (list-name opid origin timestamp)
  (assert (stringp list-name)() "You must supply a string list-name parameter; found ~S" list-name)
  (assert (integerp opid)() "You must supply an iref opid parameter; found ~S" opid)
  (assert (integerp origin)() "You must supply an iref origin parameter; found ~S" origin)
  (assert (stringp timestamp)() "You must supply a string timestamp parameter; found ~S" timestamp)
  (let ()
    (values
     (delectus::trim (format nil "

INSERT INTO `~A` (`type`, `opid`, `origin`, `timestamp`, `name`, `item`, `deleted`, `peer`) 
VALUES (?, ?, ?, ?, ?, ?, ?, ?) 

" *listdata-table-name*))
     (list "listname" opid origin timestamp list-name nil nil nil))))

;;; (sqlgen-insert-listname-op "Test" 1 0 (now-timestamp))

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-columns-op
;;; ---------------------------------------------------------------------

(defun %sqlgen-columns-data-labels (columns-data)
  (mapcar #'as-string (get-keys columns-data)))

(defun %sqlgen-columns-data-objects (columns-data)
  (get-values columns-data))

(defun %sqlgen-columns-data-insert-parameters (columns-data)
  (let ((lbls (columns-data-labels columns-data)))
    (join-strings ", " lbls)))

(defun %sqlgen-columns-data-insert-placeholders (columns-data)
  (let* ((lbls (columns-data-labels columns-data))
         (placeholders (mapcar (constantly "?")
                               lbls)))
    (join-strings ", " placeholders)))

(defun %sqlgen-columns-data-insert-values (columns-data)
  (mapcar #'to-json (columns-data-objects columns-data)))

(defun sqlgen-insert-columns-op (opid origin timestamp columns-data)
  (assert (integerp opid)() "You must supply an iref opid parameter; found ~S" opid)
  (assert (integerp origin)() "You must supply an iref origin parameter; found ~S" origin)
  (assert (stringp timestamp)() "You must supply a string timestamp parameter; found ~S" timestamp)
  (let* ((column-labels (%sqlgen-columns-data-labels columns-data))
         (column-objects (%sqlgen-columns-data-objects columns-data))
         (insert-parameters-string (%sqlgen-columns-data-insert-parameters columns-data))
         (insert-placeholders-string (%sqlgen-columns-data-insert-placeholders columns-data))
         (insert-values (%sqlgen-columns-data-insert-values columns-data)))
    (values
     (delectus::trim
      (format nil
              "

INSERT INTO `~A` (`type`, `opid`, `origin`, `timestamp`, `name`, `item`, `deleted`, `peer`, ~A) 
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ~A) 

" *listdata-table-name* insert-parameters-string insert-placeholders-string))
     `("columns" ,opid ,origin ,timestamp nil nil nil nil ,@insert-values))))

;;; (sqlgen-insert-columns-op 1 0 (now-timestamp) (make-default-columns-data))


