(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; button utils
;;; ---------------------------------------------------------------------

(defparameter $NSToggleButton 2)
(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageAbove 5)

(defun trash-button (pane view)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setButtonType:" $NSToggleButton)
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource "images/trashempty48.png"))))
  (objc:invoke view "setAlternateImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource "images/trashfull48.png"))))
  view)

(defun top-button (label image altimage pane view)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setTitle:" label)
  (objc:invoke view "setImagePosition:" $NSImageAbove)
  (objc:invoke view "setButtonType:" $NSMomentaryChangeButton)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource image))))
  (objc:invoke view "setAlternateImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource altimage))))
  view)

(defun add-row-button (pane view)
  (top-button "Add Row" "images/add.png" "images/addhl.png" pane view))

(defun delete-row-button (pane view)
  (top-button "Del Row" "images/del.png" "images/delhl.png" pane view))

(defun add-col-button (pane view)
  (top-button "Add Col" "images/add.png" "images/addhl.png" pane view))

(defun delete-col-button (pane view)
  (top-button "Del Col" "images/del.png" "images/delhl.png" pane view))

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

(defun init-row-pane (pane scrollview)
  (let* ((table-view (alloc-init-object "NSTableView"))
         (scrollview (invoke scrollview "init"))
         (source (retain (alloc-init-object "DataSource")))
         (col (retain (alloc-init-object "NSTableColumn"))))
    (invoke scrollview "setHasVerticalScroller:" t)
    (invoke scrollview "setHasHorizontalScroller:" t)
    (invoke table-view "setUsesAlternatingRowBackgroundColors:" t)
    (invoke scrollview "setDocumentView:" table-view)
    (invoke table-view "setDataSource:" source)
    (invoke table-view "addTableColumn:" col)
    scrollview))

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(defun open-file (intf)
  (format t "Open selected"))

(define-interface delectus-window ()
  ;; slots
  ((model :reader model :initarg :model :initform nil))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton":init-function 'add-row-button)
   (delete-row-button cocoa-view-pane :view-class "NSButton":init-function 'delete-row-button)
   (add-column-button cocoa-view-pane :view-class "NSButton" :init-function 'add-col-button)
   (delete-column-button cocoa-view-pane :view-class "NSButton" :init-function 'delete-col-button)
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView" :reader row-pane :init-function 'init-row-pane)
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
                     :window-styles '(:internal-borderless :textured-background)))

;;; (setq $w (contain (make-instance 'delectus-window)))
