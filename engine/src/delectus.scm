;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       The definitions in this file provide the C
;;;                 C code uses to call into the Delectus engine.
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; C interface
;;; ---------------------------------------------------------------------

(c-define (c:version) () 
          int "version" "" 
          (if-error $ERR_UNKNOWN_ERROR
                    (lambda ()(api:version))))

(c-define (c:make_document) ()
          unsigned-long "make_document" "" 
          (if-error $OBJ_NO_OID
                    (lambda ()(api:make-document))))

(c-define (c:add_row oid) (unsigned-long)
          int "add_row" ""
          (if-error $ERR_CANT_ADD_ROW
                    (lambda ()(api:add-row! oid))))

(c-define (c:add_column oid label) (unsigned-long char-string)
          int "add_column" "" 
          (if-error $ERR_CANT_ADD_COLUMN
                    (lambda ()(api:add-column! oid label))))

(c-define (c:value_at oid column-label row-index) (unsigned-long char-string int)
          char-string "value_at" "" 
          (if-error $VAL_NO_VALUE
                    (lambda ()(api:value-at oid column-label row-index))))

(c-define (c:put_value_at oid column-label row-index val) (unsigned-long char-string int char-string)
          int "put_value_at" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:put-value-at! oid column-label row-index val))))

(c-define (c:mark_column_deleted oid column-label deleted?) (unsigned-long char-string bool)
          int "mark_column_deleted" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:mark-column-deleted! oid column-label deleted?))))

(c-define (c:mark_row_deleted oid row-index deleted?) (unsigned-long int bool)
          int "mark_row_deleted" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:mark-row-deleted! oid row-index deleted?))))

(c-define (c:show_deleted oid) (unsigned-long)
          bool "show_deleted" "" 
          (if-error $VAL_NO
                    (lambda ()(api:show-deleted? oid))))

(c-define (c:set_show_deleted oid deleted?) (unsigned-long bool)
          int "set_show_deleted" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:set-show-deleted! oid deleted?))))

(c-define (c:compact_table oid) (unsigned-long)
          int "compact_table" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:compact-table! oid))))

(c-define (c:sort_column oid) (unsigned-long)
          char-string "sort_column" "" 
          (if-error $VAL_NO_VALUE
                    (lambda ()(api:sort-column oid))))

(c-define (c:set_sort_column oid column-label) (unsigned-long char-string)
          int "set_sort_column" "" 
          (if-error $VAL_NO_VALUE
                    (lambda ()(api:set-sort-column! oid column-label))))

(c-define (c:sort_order oid) (unsigned-long)
          int "sort_order" "" 
          (if-error $SORT_NONE
                    (lambda ()(api:sort-order oid))))

(c-define (c:set_sort_order oid order) (unsigned-long int)
          int "set_sort_order" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:set-sort-order! oid order))))

(c-define (c:sort_type oid) (unsigned-long)
          int "sort_type" "" 
          (if-error $SORT_NONE
                    (lambda ()(api:sort-type oid))))

(c-define (c:set_sort_type oid type) (unsigned-long int)
          int "set_sort_type" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:set-sort-type! oid type))))

(c-define (c:filter_text oid) (unsigned-long)
          char-string "filter_text" "" 
          (if-error $VAL_NO_VALUE
                    (lambda ()(api:filter-text oid))))

(c-define (c:set_filter_text oid text) (unsigned-long char-string)
          int "set_filter_text" "" 
          (if-error $ERR_CANT_UPDATE
                    (lambda ()(api:set-filter-text! oid text))))

(c-define (c:write_delectus oid path) (unsigned-long char-string)
          int "write_delectus" "" 
          (if-error $ERR_CANT_WRITE
                    (lambda ()(api:write-delectus oid path))))

(c-define (c:read_delectus path) (unsigned-long char-string)
          unsigned-long "read_delectus" "" 
          (if-error $OBJ_NO_OID
                    (lambda ()(api:read-delectus path))))

(c-define (c:write_delectus_csv oid path) (unsigned-long char-string)
          int "write_delectus_csv" "" 
          (if-error $ERR_CANT_WRITE
                    (lambda ()(api:write-delectus/csv oid path))))

(c-define (c:read_delectus_csv path) (unsigned-long char-string)
          unsigned-long "read_delectus_csv" "" 
          (if-error $OBJ_NO_OID
                    (lambda ()(api:read-delectus/csv path))))
