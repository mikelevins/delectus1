;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       the C interface to the Delectus API
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(c-define (c:version) () 
          char-string "version" ""
          (api:version))

(c-define (c:new-delectus) () 
          int "new_delectus" ""
          (api:new-delectus))

(c-define (c:get-view id include-deleted? sort-column sort-order filter-text) (int int int int char-string)
          int "get_view" ""
          (api:get-view id include-deleted? sort-column sort-order filter-text))

(c-define (c:value-at id column-label row-index) (int char-string int) 
          char-string "value_at" ""
          (api:value-at id column-label row-index))

(c-define (c:put-value-at id column-label row-index val) (int char-string int char-string) 
          int "put_value_at" ""
          (api:put-value-at! id column-label row-index val))

(c-define (c:add-row! id) (int) 
          int "add_row" ""
          (api:add-row id))

(c-define (c:add-column! id column-label) (int char-string) 
          int "add_column" ""
          (api:add-column! id column-label))

(c-define (c:mark-row-deleted! id row-index deleted?) (int int int)
          int "mark_row_deleted" ""
          (api:mark-row-deleted! id row-index deleted?))

(c-define (c:compact-delectus! id)(int)
          int "compact_delectus" ""
          (api:compact-delectus! id))

(c-define (c:write-delectus-file id path)(int char-string)
          int "write_delectus_file" ""
          (api:write-delectus-file id path))

(c-define (c:read-delectus-file path)(char-string)
          int "read_delectus_file" ""
          (api:read-delectus-file path))

(c-define (c:write-delectus-csv id path)(int char-string)
          int "write_delectus_csv" ""
          (api:write-delectus-csv id path))

(c-define (c:read-delectus-csv path)(char-string)
          int "read_delectus_csv" ""
          (api:read-delectus-csv path))
