(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; control utils
;;; ---------------------------------------------------------------------

(defparameter $tags->control-descriptions (map:make))

(defun tag (ctrl)(invoke ctrl "tag"))
(defun set-tag (ctrl tag)(invoke ctrl "setTag:" tag))

(defun control-description (&key capi-pane objective-c-view tag interface)
  (map:make :capi-pane capi-pane
            :objective-c-view objective-c-view
            :tag (or tag (next-widget-id))
            :interface interface))

(defun find-control (tag)
  (map:get $tags->control-descriptions tag :default nil))

(defun register-control-description (desc)
  (setf $tags->control-descriptions
        (map:associate $tags->control-descriptions (map:get desc :tag) desc))
  (map:get desc :tag))

;;; ---------------------------------------------------------------------
;;; cocoa utils
;;; ---------------------------------------------------------------------

(defun ns-image (path)
  (invoke (invoke "NSImage" "alloc") "initByReferencingFile:" path))

;;; ---------------------------------------------------------------------
;;; menu setup
;;; ---------------------------------------------------------------------

(defun open-file (intf)
  (format t "Open selected"))

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
;;; button setup
;;; ---------------------------------------------------------------------

(defparameter $NSToggleButton 2)
(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageAbove 5)

(defun init-cocoa-button (&key interface capi-pane objective-c-view tag
                          target button-type action bordered image alt-image)
  (setf objective-c-view (invoke objective-c-view "init"))
  (set-tag objective-c-view tag)
  (invoke objective-c-view "setTarget:" target)
  (invoke objective-c-view "setButtonType:" button-type)
  (invoke objective-c-view "setAction:" action)
  (invoke objective-c-view "setBordered:" bordered)
  (invoke objective-c-view "setImage:" image)
  (invoke objective-c-view "setAlternateImage:" alt-image)
  (register-control-description (control-description :capi-pane capi-pane :tag tag :interface interface
                                                     :objective-c-view objective-c-view))
  objective-c-view)

(defun trash-button (intf capi-pane objc-view)
  (init-cocoa-button :interface intf :capi-pane capi-pane 
                     :objective-c-view objc-view :tag (next-widget-id)
                     :target (objc-object-pointer (app-delegate)) 
                     :action (coerce-to-selector "toggleTrash:")
                     :button-type $NSToggleButton :bordered nil
                     :image (ns-image (namestring (resource "images/trashempty48.png")))
                     :alt-image (ns-image (namestring (resource "images/trashfull48.png")))))


(defun top-button (intf label action image altimage capi-pane objc-view)
  (let ((btn (init-cocoa-button :interface intf :capi-pane capi-pane
                                :objective-c-view objc-view :tag (next-widget-id)
                                :target (objc-object-pointer (app-delegate)) 
                                :action (coerce-to-selector action)
                                :button-type $NSMomentaryChangeButton :bordered nil
                                :image image :alt-image altimage)))
    (invoke btn "setTitle:" label)
    (invoke btn "setImagePosition:" $NSImageAbove)
    btn))

(defun add-row-button (intf capi-pane objc-view)
  (top-button intf "Add Row" "addRow:" 
              (ns-image (namestring (resource "images/add.png")))
              (ns-image (namestring (resource "images/addhl.png")))
              capi-pane objc-view))

(defun delete-row-button (intf capi-pane objc-view)
  (top-button intf "Del Row" "deleteRow:" 
              (ns-image (namestring (resource "images/del.png")))
              (ns-image (namestring (resource "images/delhl.png")))
              capi-pane objc-view))

(defun add-col-button (intf capi-pane objc-view)
  (top-button intf "Add Col" "addColumn:" 
              (ns-image (namestring (resource "images/add.png")))
              (ns-image (namestring (resource "images/addhl.png")))
              capi-pane objc-view))

(defun delete-col-button (intf capi-pane objc-view)
  (top-button intf "Del Col" "deleteColumn:" 
              (ns-image (namestring (resource "images/del.png")))
              (ns-image (namestring (resource "images/delhl.png")))
              capi-pane objc-view))


;;; ---------------------------------------------------------------------
;;; NSTableView setup
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

