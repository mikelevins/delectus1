(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus ui
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(define-interface delectus-window ()
  ;; slots
  ((document :reader document))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton")
   (delete-row-button cocoa-view-pane :view-class "NSButton")
   (add-column-button cocoa-view-pane :view-class "NSButton")
   (delete-column-button cocoa-view-pane :view-class "NSButton")
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView")
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton")
   (filter-field cocoa-view-pane :view-class "NSSearchField"))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster) :external-min-height 60 :external-max-height 60)
   (table-row row-layout '(row-pane))
   (bottom-row row-layout '(trash-cluster nil filter-field) :external-min-height 56 :external-max-height 56)
   ;; control clusters
   (row-cluster row-layout '(add-row-button delete-row-button) :adjust :center 
                :external-min-width 196 :external-max-width 196)
   (column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
                   :external-min-width 196 :external-max-width 196)
   (trash-cluster row-layout '(trash-button) :adjust :center
                  :external-min-width 84 :external-max-width 84))
  ;; menus
  (:menus
   (file-menu "File" () :items-function 'file-menu-items)
   (edit-menu "Edit" () :items-function 'edit-menu-items)
   (windows-menu "Window" () :items-function 'windows-menu-items)
   (help-menu "Help" () :items-function 'help-menu-items))
  ;; menubar
  (:menu-bar file-menu edit-menu windows-menu help-menu)
  ;; defaults
  (:default-initargs :title "Delectus" :width 700 :height 400 :initial-focus 'filter-field
                     :window-styles '(:internal-borderless :textured-background)))

(defmethod initialize-instance :after ((dw delectus-window) &rest initargs &key (document nil)
                                       &allow-other-keys)
  (let ((doc (or document (make-instance 'document))))
    (setf (slot-value dw 'document)  doc)))

