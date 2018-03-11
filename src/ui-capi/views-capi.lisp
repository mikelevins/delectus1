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
;;; CLASS sqlite-window
;;; ---------------------------------------------------------------------
;;; a window used to inspect the contents of SQLite files

;;; callbacks
(defun handle-sqlite-table-selection (table-name sqlite-window)
  (let* ((controller (controller sqlite-window))
         (dbpath (dbpath controller))
         (column-names (mapcar #'second
                               (delectus.data::sqlite-list-table-columns (dbpath (controller sqlite-window))
                                                                         table-name)))
         (columns-pane (columns-pane sqlite-window)))
    (when column-names
      (setf (collection-items columns-pane)
            column-names))))

;;; interface definition
(define-interface sqlite-window ()
  ;; -- slots ---------------------------------------------
  ((controller :accessor controller :initform nil :initarg :controller))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane
                :keep-selection-p nil
                :selection-callback 'handle-sqlite-table-selection
                :callback-type :item-interface)
   (columns-pane list-panel :reader columns-pane)
   (contents-pane multi-column-list-panel :reader contents-pane
                  :columns (compute-column-descriptions interface)
                  :items (compute-visible-rows interface)
                  :alternating-background t
                  :auto-reset-column-widths nil))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (browser-layout column-layout '(tables-pane :divider columns-pane))
   (main-layout row-layout '(browser-layout :divider contents-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :title "SQLite Utilities"
    :window-styles '(:textured-background)
    :width 800 :height 600))

(defmethod initialize-instance :after ((win sqlite-window) &rest initargs &key &allow-other-keys)
  (when (controller win)
    (when (dbpath (controller win))
      (let ((table-names (delectus.data::sqlite-list-tables (dbpath (controller win))))
            (tables-pane (tables-pane win)))
        (when table-names
          (setf (collection-items tables-pane)
                table-names)
          (setf (choice-selection tables-pane)
                nil))))))


;;; ---------------------------------------------------------------------
;;; compute-column-descriptions (window)
;;; ---------------------------------------------------------------------
;;; *private generic function*
;;;
;;; Returns a plist of column descriptions suitable for passing
;;; as an init arg to the initializer for `contents-pane`, a
;;; `multi-column-list-pane`.
;;; TODO: a real version; this just always computes columns for the "contents" table

(defmethod compute-column-descriptions ((window sqlite-window))
  (let* ((column-labels (delectus.data::sqlite-list-table-columns (dbpath (controller window)) "contents")))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))

;;; ---------------------------------------------------------------------
;;; compute-visible-rows (window)
;;; ---------------------------------------------------------------------
;;; *private generic function*
;;;
;;; Returns the rows from `document` that are currently visible,
;;; taking into account the current state of `window`.
;;; TODO: a real version; this just always computes rows for the "contents" table

(defmethod compute-visible-rows ((window sqlite-window))
  (delectus.data::sqlite-get-table-rows (dbpath (controller window)) "contents" :from 0 :count 10))
