(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; control utils
;;; ---------------------------------------------------------------------

(defun tag (ctrl)(invoke ctrl "tag"))
(defun set-tag (ctrl tag)(invoke ctrl "setTag:" tag))

;;; ---------------------------------------------------------------------
;;; button utils
;;; ---------------------------------------------------------------------

(defparameter $NSToggleButton 2)
(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageAbove 5)

(defun trash-button (pane view)
  (setf view (invoke view "init"))
  (set-tag view (next-widget-id))
  (invoke view "setButtonType:" $NSToggleButton)
  (invoke view "setBordered:" nil)
  (invoke view "setTarget:" (objc-object-pointer (app-delegate)))
  (invoke view "setAction:" (coerce-to-selector "toggleTrash:"))
  (invoke view "setImage:"
          (invoke (invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource "images/trashempty48.png"))))
  (invoke view "setAlternateImage:"
               (invoke (invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource "images/trashfull48.png"))))
  view)

(defun top-button (label image altimage pane view)
  (setf view (invoke view "init"))
  (set-tag view (next-widget-id))
  (invoke view "setBordered:" nil)
  (invoke view "setTitle:" label)
  (invoke view "setImagePosition:" $NSImageAbove)
  (invoke view "setButtonType:" $NSMomentaryChangeButton)
  (invoke view "setImage:"
               (invoke (invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource image))))
  (invoke view "setAlternateImage:"
               (invoke (invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource altimage))))
  view)

(defun add-row-button (pane view)
  (let ((btn (top-button "Add Row" "images/add.png" "images/addhl.png" pane view)))
    (set-tag btn (next-widget-id))
    (invoke btn "setTarget:" (objc-object-pointer (app-delegate)))
    (invoke btn "setAction:" (coerce-to-selector "addRow:"))
    btn))

(defun delete-row-button (pane view)
  (let ((btn (top-button "Del Row" "images/del.png" "images/delhl.png" pane view)))
    (set-tag btn (next-widget-id))
    (invoke btn "setTarget:" (objc-object-pointer (app-delegate)))
    (invoke btn "setAction:" (coerce-to-selector "deleteRow:"))
    btn))

(defun add-col-button (pane view)
  (let ((btn (top-button "Add Col" "images/add.png" "images/addhl.png" pane view)))
    (invoke btn "setTag:" (next-widget-id))
    (invoke btn "setTarget:" (objc-object-pointer (app-delegate)))
    (invoke btn "setAction:" (coerce-to-selector "addColumn:"))
    btn))

(defun delete-col-button (pane view)
  (let ((btn (top-button "Del Col" "images/del.png" "images/delhl.png" pane view)))
    (set-tag btn (next-widget-id))
    (invoke btn "setTarget:" (objc-object-pointer (app-delegate)))
    (invoke btn "setAction:" (coerce-to-selector "deleteColumn:"))
    btn))

;;; ---------------------------------------------------------------------
;;; menus
;;; ---------------------------------------------------------------------

(defun file-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "New"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Open"
                                                   :callback 'open-file :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Close"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save As"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Revert to Saved"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Empty Trash"
                                                   :callback 'open-file :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Page Setup"
                                                   :callback 'open-file :callback-type :interface)
                               (make-instance 'capi:menu-item :text "Print"
                                              :callback 'open-file :callback-type :interface)))))

(defun edit-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Undo"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Redo"
                                                   :callback 'open-file :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Cut"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Copy"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Paste"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Delete"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Select All"
                                                   :callback 'open-file :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Rename Column"
                                                   :callback 'open-file :callback-type :interface)))))

(defun windows-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Minimize"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Zoom"
                                                   :callback 'open-file :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Bring All To Front"
                                                   :callback 'open-file :callback-type :interface)))))

(defun help-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Delectus Help"
                                                   :callback 'open-file :callback-type :interface)))))

;;; ---------------------------------------------------------------------
;;; view utils
;;; ---------------------------------------------------------------------

(defun init-row-pane (intf pane scrollview)
  (let* ((table-view (alloc-init-object "NSTableView"))
         (scrollview (invoke scrollview "init"))
         (source (source intf)))
    (invoke scrollview "setHasVerticalScroller:" t)
    (invoke scrollview "setHasHorizontalScroller:" t)
    (invoke table-view "setUsesAlternatingRowBackgroundColors:" t)
    (invoke scrollview "setDocumentView:" table-view)
    (invoke table-view "setDataSource:" (objc-object-pointer source))
    scrollview))

(defun add-table-column (intf scrollview column-name)
  (let ((table-view (invoke (cocoa-view-pane-view scrollview) "documentView"))
        (table-column (invoke (invoke "NSTableColumn" "alloc") "initWithIdentifier:" column-name)))
    (invoke (invoke table-column "headerCell") "setStringValue:" column-name)
    (invoke table-view "addTableColumn:" table-column)
    (invoke table-view "reloadData")))

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(defun open-file (intf)
  (format t "Open selected"))

(define-interface delectus-window ()
  ;; slots
  ((source :reader source :initarg :source 
           :initform (make-instance 'data-source :model (make-instance 'delectus-model))))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton":init-function 'add-row-button)
   (delete-row-button cocoa-view-pane :view-class "NSButton":init-function 'delete-row-button)
   (add-column-button cocoa-view-pane :view-class "NSButton" :init-function 'add-col-button 
                      :reader add-column-button)
   (delete-column-button cocoa-view-pane :view-class "NSButton" :init-function 'delete-col-button)
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView" :reader row-pane 
             :init-function (fun:partial 'init-row-pane interface))
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton" :init-function 'trash-button)
   (filter-field cocoa-view-pane :view-class "NSSearchField"))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster)
            :external-min-height 60 :external-max-height 60)
   (table-row row-layout '(row-pane))
   (bottom-row row-layout '(trash-cluster nil filter-field)
            :external-min-height 56 :external-max-height 56)
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
                     :window-styles '(:internal-borderless :textured-background)
                     :create-callback (lambda (intf)
                                        (register-tag->interface
                                         (tag (cocoa-view-pane-view (add-column-button intf)))
                                         intf))))

;;; (setq $w (contain (make-instance 'delectus-window)))
