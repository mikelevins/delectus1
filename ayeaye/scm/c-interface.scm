;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c-interface.scm
;;;; Project:       Delectus
;;;; Purpose:       The definitions in this file provide the C
;;;                 entry points that Objective-C code uses to call into
;;;                 the Delectus Scheme back-end.
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-declare "#import <Cocoa/Cocoa.h>")

;;; ----------------------------------------------------------------------
;;; support for NSDocument APIs in class DelectusDocument
;;; ----------------------------------------------------------------------

(c-define (c:read-csv-from-path p headers-in-first-line?) (char-string bool) 
          int 
          "read_csv_from_path" ""
          (api:read-csv-from-path p headers-in-first-line?))

(c-define (c:read-delectus-from-path p) (char-string) 
          int 
          "read_delectus_from_path" ""
          (api:read-delectus-from-path p))

(c-define (c:write-csv-to-path src dest) (int char-string) 
          int 
          "write_csv_to_path" ""
          (api:write-csv-to-path src dest))

(c-define (c:write-delectus-to-path src dest) (int char-string) 
          int 
          "write_delectus_to_path" ""
          (api:write-delectus-to-path src dest))

;;; ----------------------------------------------------------------------
;;; support for NSDocument IBActions in class DelectusDocument
;;; ----------------------------------------------------------------------

(c-define (c:set-filter document-id filter-string) (int char-string ) 
          void 
          "set_filter" ""
          (api:set-filter document-id filter-string))

(c-define (c:advance-sort! document-id label) (int char-string) 
          bool 
          "advance_sort" ""
          (api:advance-sort! document-id label))

(c-define (c:add-column document-id label) (int char-string ) 
          void 
          "add_column" ""
          (api:add-column document-id label))

(c-define (c:rename-column! document-id old-label new-label) (int char-string char-string) 
          void 
          "rename_column" ""
          (api:rename-column! document-id old-label new-label))

(c-define (c:add-row document-id) (int) 
          void 
          "add_row" ""
          (api:add-row document-id))

(c-define (c:toggle-column-deleted document-id label) (int char-string ) 
          void 
          "toggle_column_deleted" ""
          (api:toggle-column-deleted! document-id label))

;;; the row is identified by index into the displayed rows. In other words, the indicated row
;;; is looked up in the filtered and sorted rows, not in the DB's list of all rows
(c-define (c:toggle-row-deleted document-id row-index) (int  int) 
          void 
          "toggle_row_deleted" ""
          (api:toggle-row-deleted! document-id row-index))

(c-define (c:toggle-show-deleted document-id) (int) 
          void 
          "toggle_show_deleted" ""
          (api:toggle-show-deleted! document-id))

(c-define (c:empty-trash document-id) (int) 
          void 
          "empty_trash" ""
          (api:empty-trash! document-id))

;;; ----------------------------------------------------------------------
;;; support for Data source methods in class DelectusDocument
;;; ----------------------------------------------------------------------

(c-define (c:number-of-columns document-id) (int) 
          int
          "number_of_columns" ""
          (api:number-of-columns document-id))

;;; returns a count of the filtered and sorted rows (not a count of *all* rows)
(c-define (c:number-of-rows document-id) (int) 
          int
          "number_of_rows" ""
          (api:number-of-rows document-id))

;;; the row is identified by index into the displayed rows. In other words, the indicated row
;;; is looked up in the filtered and sorted rows, not in the DB's list of all rows
(c-define (c:value-for-cell document-id column-label row-index) (int char-string  int) 
          (pointer "NSString")
          "value_for_cell" ""
          (api:value-for-cell document-id column-label row-index))

;;; the row is identified by index into the displayed rows. In other words, the indicated row
;;; is looked up in the filtered and sorted rows, not in the DB's list of all rows
(c-define (c:set-value-for-cell! document-id column-label row-index val) 
          (int char-string int char-string ) 
          void
          "set_value_for_cell" ""
          (api:set-value-for-cell! document-id column-label row-index val))

;;; ----------------------------------------------------------------------
;;; support for NSTableView display in class DelectusDocument
;;; ----------------------------------------------------------------------

(c-define (c:get-new-document) () 
          int
          "get_new_document" ""
          (api:get-new-document))

(c-define (c:get-column-labels document-id) (int) 
          (pointer "NSMutableArray")
          "get_column_labels" ""
          (api:get-column-labels document-id))

(c-define (c:is-column-deleted document-id label) (int char-string ) 
          bool
          "is_column_deleted" ""
          (api:is-column-deleted? document-id label))

(c-define (c:get-column-width document-id label) (int char-string ) 
          int
          "get_column_width" ""
          (inexact->exact (round (api:get-column-width document-id label))))

(c-define (c:set-column-width! document-id label new-width) (int char-string int) 
          void
          "set_column_width" ""
          (api:set-column-width! document-id label new-width))

(c-define (c:move-column! document-id label new-index) (int char-string int) 
          void
          "move_column" ""
          (api:move-column! document-id label new-index))

(c-define (c:is-row-deleted document-id row-index) (int int) 
          bool
          "is_row_deleted" ""
          (api:is-row-deleted? document-id row-index))

(c-define (c:are-deleted-items-shown? document-id) (int) 
          bool 
          "are_deleted_items_shown" ""
          (api:are-deleted-items-shown? document-id))

(c-define (c:has-deleted-items? document-id) (int) 
          bool
          "has_deleted_items" ""
          (api:has-deleted-items? document-id))

(c-define (c:is-duplicate-label? document-id label) (int char-string) 
          bool
          "is_duplicate_label" ""
          (api:is-duplicate-label? document-id label))
