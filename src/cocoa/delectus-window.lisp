(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(define-interface delectus-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Add Row" :bordered nil
                                               :image (ns-image (resource "add.png"))
                                               :altimage (ns-image (resource "addhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (delete-row-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Del Row" :bordered nil
                                               :image (ns-image (resource "del.png"))
                                               :altimage (ns-image (resource "delhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (add-column-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Add Col" :bordered nil
                                               :image (ns-image (resource "add.png"))
                                               :altimage (ns-image (resource "addhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   (delete-column-button cocoa-view-pane :view-class "NSButton"
                   :init-function (button-init :target (document interface) :label "Del Col" :bordered nil
                                               :image (ns-image (resource "del.png"))
                                               :altimage (ns-image (resource "delhl.png"))
                                               :button-type $NSMomentaryChangeButton))
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView" :reader row-pane
             :init-function (table-init :data-source (document interface)))
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton"
                 :init-function (button-init :target (document interface) :label "Trash" :bordered nil
                                             :image (ns-image (resource "trashempty48.png"))
                                             :image-position $NSImageOnly
                                             :altimage (ns-image (resource "trashfull48.png"))
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
  ;; defaults
  (:default-initargs :title "Delectus" :width 700 :height 400 :initial-focus 'filter-field
                     :window-styles '(:internal-borderless :textured-background)
                     :create-callback (lambda (intf)
                                        (let* ((doc (document intf)))
                                          (when doc
                                            (setup-columns (row-pane intf) doc))))
                     :activate-callback (lambda (intf activep)
                                          (when activep
                                            (unless (eql (active-interface (app)) intf)
                                              (setf (active-interface (app)) intf))))
                     :destroy-callback (lambda (intf)
                                         (when (eql (active-interface (app)) intf)
                                           (setf (active-interface (app)) nil)))))


