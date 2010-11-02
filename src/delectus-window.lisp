(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus ui
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(define-interface delectus-window ()
  ;; slots
  ((document :reader document :initarg :document))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Add Row" :bordered nil
                                               :image (ns-image (resource "images/add.png"))
                                               :altimage (ns-image (resource "images/addhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (delete-row-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Del Row" :bordered nil
                                               :image (ns-image (resource "images/del.png"))
                                               :altimage (ns-image (resource "images/delhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (add-column-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Add Col" :bordered nil
                                               :image (ns-image (resource "images/add.png"))
                                               :altimage (ns-image (resource "images/addhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (delete-column-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Del Col" :bordered nil
                                               :image (ns-image (resource "images/del.png"))
                                               :altimage (ns-image (resource "images/delhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView"
             :init-function (table-init :data-source (document interface)))
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton"
                 :init-function (button-init :target (document interface) :label "Trash" :bordered nil
                                             :image (ns-image (resource "images/trashempty48.png"))
                                             :image-position $NSImageOnly
                                             :altimage (ns-image (resource "images/trashfull48.png"))
                                             :button-type $NSToggleButton))
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


