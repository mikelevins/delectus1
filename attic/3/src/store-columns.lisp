;;;; ***********************************************************************
;;;;
;;;; Name:          store-columns.lisp
;;;; Project:       delectus 2
;;;; Purpose:       working with columns
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; columns
;;; =====================================================================

(defmethod db-ensure-columns-exist ((db sqlite-handle) column-descriptions)
  (let* ((supplied-column-labels (mapcar (lambda (desc)(getf desc :|label|))
                                         column-descriptions))
         (columns-column-labels (mapcar 'column-info-name
                                        (db-sqlite-table-column-info db *columns-table-name*)))
         (items-column-labels (mapcar 'column-info-name
                                      (db-sqlite-table-column-info db *items-table-name*)))
         (missing-columns-column-labels (remove-list-elements columns-column-labels supplied-column-labels))
         (missing-items-column-labels (remove-list-elements items-column-labels supplied-column-labels)))
    (when missing-columns-column-labels
      (loop for label in missing-columns-column-labels
         do (bind ((sql vals (sqlgen-add-columns-userdata-column label)))
              (apply 'execute-non-query db sql vals))))
    (when missing-items-column-labels
      (loop for label in missing-items-column-labels
         do (bind ((sql vals (sqlgen-add-items-userdata-column label)))
              (apply 'execute-non-query db sql vals))))))
