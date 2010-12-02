;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-list-pane.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the pane that displays Delectus columns
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(define-interface delectus-list-pane ()
  ()
  ;; panes
  (:panes)
  ;; layouts
  (:layouts
   (main-layout simple-pinboard-layout '() :reader contents))
  ;; defaults
  (:default-initargs))

(defun max-item-length (rows col-index)
  (let ((result 0))
    (loop for row in rows
       do (setf result (max result (length (elt row col-index)))))
    result))

(defun column-description (pres col)
  (let* ((col-index (column-index pres col))
         (col-width (max-item-length (rows pres)
                                     (column-index pres col))))
    (list :title col :adjust :left :width (list 'character (max (length col)
                                                                (+ col-width 2))))))
#+cocoa
(defmethod %get-nstableview ((pane multi-column-list-panel))
  (slot-value (slot-value pane 'capi-internals::representation)
              'capi-cocoa-library::main-view))

#+cocoa
(defun %setup-nstableview (pane)
  (let ((objc-view (%get-nstableview pane)))
    (objc:invoke objc-view "setAllowsColumnReordering:" t)
    (objc:invoke objc-view "setUsesAlternatingRowBackgroundColors:" t)))

(defmethod update-presentation! ((pane delectus-list-pane)(pres presentation))
  (update pres)
  (let ((row-pane (make-instance 'multi-column-list-panel
                                 :columns (mapcar (fun:partial #'column-description pres)
                                                  (columns pres))
                                 :items (rows pres))))
    (setf (layout-description (contents pane))
          (list row-pane))
    #+cocoa (%setup-nstableview row-pane)))

(defmethod ensure-item-is-visible ((pane delectus-list-pane)(col-index integer)(row-index integer))
  #+cocoa
  (let ((objc-view (%get-nstableview (first (layout-description (contents pane))))))
    (objc:invoke objc-view "scrollColumnToVisible:" col-index)
    (objc:invoke objc-view "scrollRowToVisible:" row-index))
  #+win32 ())

