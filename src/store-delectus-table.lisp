;;;; ***********************************************************************
;;;;
;;;; Name:          store-delectus-table.lisp
;;;; Project:       delectus 2
;;;; Purpose:       creating the delectus table
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; creating the 'delectus' table
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle) (listid string)(format string))
  (assert (identity-string? listid)()
          "Expected an identity-string for the :LISTID paramter, but found ~S"
          listid)
  (bind ((create-sql create-vals (sqlgen-create-delectus-table))
         (init-sql init-vals (sqlgen-init-delectus-table listid :format format)))
    (apply 'execute-non-query db create-sql create-vals)
    (apply 'execute-non-query db init-sql init-vals)))

;;; ---------------------------------------------------------------------
;;; operations on the 'delectus' table
;;; ---------------------------------------------------------------------

(defmethod db-get-next-revision ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-revision)))
    (apply 'execute-single db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-get-next-revision db))

(defmethod db-set-next-revision ((db sqlite-handle)(rev integer))
  (bind ((sql vals (sqlgen-set-next-revision rev)))
    (apply 'execute-single db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-set-next-revision db 1))


(defmethod db-get-next-itemid ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-itemid)))
    (apply 'execute-single db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-get-next-itemid db))

(defmethod db-set-next-itemid ((db sqlite-handle)(it integer))
  (bind ((sql vals (sqlgen-set-next-itemid it)))
    (apply 'execute-single db sql vals)))

