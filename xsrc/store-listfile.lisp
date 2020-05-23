;;;; ***********************************************************************
;;;;
;;;; Name:          store-listfile.lisp
;;;; Project:       delectus 2
;;;; Purpose:       creating, reading, and writing the listfile
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; the 'delectus' table
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating it 
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle) (listid string)(format string))
  (assert (identity-string? listid)()
          "Expected an identity-string for the :LISTID paramter, but found ~S"
          listid)
  (bind ((create-sql create-vals (sqlgen-create-delectus-table))
         (init-sql init-vals (sqlgen-init-delectus-table listid :format format)))
    (apply 'execute-non-query db create-sql create-vals)
    (apply 'execute-non-query db init-sql init-vals)))

;;; (setf $listid (make-identity-string))
;;; (setf $testlist-path (path "/Users/mikel/Desktop/testlist.delectus2"))
;;; (with-open-database (db $testlist-path) (db-create-delectus-table db $listid +delectus-format-version+))
;;; (delete-file $testlist-path)

;;; =====================================================================
;;; the 'oplog' table
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating it 
;;; ---------------------------------------------------------------------

(defmethod db-create-oplog-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-oplog-table)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; (setf $testlist-path (path "/Users/mikel/Desktop/testlist.delectus2"))
;;; (with-open-database (db $testlist-path) (db-create-oplog-table db))
;;; (delete-file $testlist-path)

;;; ---------------------------------------------------------------------
;;; operations on the 'oplog' table
;;; ---------------------------------------------------------------------

(defmethod db-insert-op ((db sqlite-handle)(timestamp integer)(hash string)(data string))
  (bind ((sql vals (sqlgen-insert-op timestamp hash data)))
    (apply 'execute-non-query db sql vals)))
