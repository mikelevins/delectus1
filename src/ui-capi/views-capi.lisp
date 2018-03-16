;;;; ***********************************************************************
;;;;
;;;; Name:          views-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Lispworks CAPI views
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.desktop)

;;; ---------------------------------------------------------------------
;;; INTERFACE sqlite-browser
;;; ---------------------------------------------------------------------
;;; the sqlite-browser is a utility for browsing the contents of
;;; arbitrary SQLite files. It is not the Delectus2 user interface;
;;; it's a tool for inspecting SQLite files.

(define-interface sqlite-browser ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (table-name :accessor table-name :initform nil :initarg :table-name)
   (rows-per-page :accessor rows-per-page :initform 10 :initarg :rows-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane
                :alternating-background t
                :items (compute-sqlite-tables interface)
                :callback-type :item-interface
                :selection-callback 'handle-table-selection)
   (rows-pane multi-column-list-panel :reader rows-pane
                :alternating-background t
                :items nil
                :columns '((:title ""))
                :callback-type :item-interface
                :selection-callback 'handle-row-selection))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout row-layout '(tables-pane :divider rows-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 600 :height 400
    :title "SQLite Browser"))

(defmethod initialize-instance :after ((browser sqlite-browser) &rest initargs &key &allow-other-keys)
  ;; the browser starts with nothing selected and waits for the user to make a selection
  (setf (choice-selection (tables-pane browser))
        nil))

;;; GENERIC FUNCTION compute-sqlite-tables interface
;;; ---------------------------------------------------------------------
;;; computes and returns the names of the tables stored in the
;;; SQLite file named by the DBPATH slot of the browser

(defmethod compute-sqlite-tables ((interface sqlite-browser))
  (let ((dbpath (dbpath interface)))
    (if dbpath
        (delectus.data::sqlite-list-tables dbpath)
      nil)))

;;; GENERIC FUNCTION compute-sqlite-columns interface
;;; ---------------------------------------------------------------------
;;; computes and returns the names of the columns of the named table

(defmethod compute-sqlite-columns ((interface sqlite-browser))
  (let* ((dbpath (dbpath interface))
         (table-name (table-name interface)))
    (if dbpath
        (if table-name
            (delectus.data::sqlite-list-table-column-names dbpath table-name)
          nil)
      nil)))

;;; GENERIC FUNCTION compute-sqlite-rows interface
;;; ---------------------------------------------------------------------
;;; computes and returns rows of the named table according to the
;;; values of the current-page and rows-per-page slots of INTERFACE

(defmethod compute-sqlite-rows ((interface sqlite-browser))
  (let* ((dbpath (dbpath interface))
         (table-name (table-name interface))
         (row-count (rows-per-page interface))
         (start-index (* row-count (current-page interface))))
    (if dbpath
        (if table-name
            (delectus.data::sqlite-get-table-rows dbpath table-name :from start-index :count row-count)
          nil)
      nil)))

;;; FUNCTION handle-table-selection
;;; ---------------------------------------------------------------------
;;; a callback function called when a table is selected by the user.
;;; it updates the TABLE-NAME slot with the selected table name

(defun handle-table-selection (item interface)
  (setf (table-name interface) item)
  (let* ((columns (compute-sqlite-columns interface))
         (column-specs (mapcar (lambda (cname) `(:title ,cname))
                               columns))
         (rows (compute-sqlite-rows interface)))
    (modify-multi-column-list-panel-columns (rows-pane interface)
                                            :columns column-specs)
    (setf (collection-items (rows-pane interface))
          rows)))

;;; (defparameter $dbpath "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; (defparameter $win (contain (make-instance 'sqlite-browser :dbpath $dbpath)))
