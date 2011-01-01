;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c-interface.scm
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
          int "version" "" (api:version))

(c-define (c:make-delectus) () 
          long "make_delectus" "" (api:make-delectus))

(c-define (c:add-row del) (long) 
          int "add_row" "" (api:add-row! del))

(c-define (c:add-column del label) (long char-string) 
          int "add_column" "" (api:add-column! del label))

(c-define (c:value-at del label index) (long char-string int) 
          char-string "value_at" "" (api:value-at del label index))

(c-define (c:put-value-at del label index val) (long char-string int char-string) 
          int "put_value_at" "" (api:put-value-at! del label index val))

(c-define (c:mark-column-deleted del label deleted?) (long char-string bool) 
          int "mark_column_deleted" "" (api:mark-column-deleted! del label deleted?))

(c-define (c:mark-row-deleted del index deleted?) (long int bool) 
          int "mark_row_deleted" "" (api:mark-row-deleted! del index deleted?))

(c-define (c:show-deleted del) (long) 
          bool "show_deleted" "" (api:show-deleted del))

(c-define (c:set-show-deleted del deleted?) (long bool) 
          int "set_show_deleted" "" (api:set-show-deleted! del deleted?))

(c-define (c:compact-delectus del) (long) 
          int "compact_delectus" "" (api:compact-delectus! del))

(c-define (c:sort-column del) (long) 
          char-string "sort_column" "" (api:sort-column del))

(c-define (c:set-sort-column! del label) (long char-string) 
          int "set_sort_column" "" (api:set-sort-column! del label))

(c-define (c:sort-order del) (long) 
          int "sort_order" "" (api:sort-order del))

(c-define (c:set-sort-order! del order) (long int) 
          int "set_sort_order" "" (api:set-sort-order! del order))

(c-define (c:sort-type del) (long) 
          int "sort_type" "" (api:sort-type del))

(c-define (c:set-sort-type! del type) (long int) 
          int "set_sort_type" "" (api:set-sort-type! del type))

(c-define (c:filter-text del) (long) 
          char-string "filter_text" "" (api:filter-text del))

(c-define (c:set-filter-text! del text) (long char-string) 
          int "set_filter_text" "" (api:set-filter-text! del text))

(c-define (c:write-delectus del path) (long char-string) 
          int "write_delectus" "" (api:write-delectus del))

(c-define (c:read-delectus path) (char-string) 
          long "read_delectus" "" (api:read-delectus del))

(c-define (c:write-delectus-csv del path) (long char-string) 
          int "write_delectus_csv" "" (api:write-delectus/csv del))

(c-define (c:read-delectus-csv path) (char-string) 
          long "read_delectus_csv" "" (api:read-delectus/csv del))
