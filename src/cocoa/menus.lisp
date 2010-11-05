(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus menus
;;; ---------------------------------------------------------------------

(defun default-menu-selected (intf)
  (warn "Menu selected for ~S" intf))

(defun delectus-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Delectus(TM) 1.9a"))

(defun delectus-preferences ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Soon to be a Preferences window"))

(defun save-menu-selected (intf)
  (multiple-value-bind (filename successp filter-name)
      (prompt-for-file "Save in:" :filter "*.delectus" :operation :save)
    (when successp
      (save-model (get-model (presentation (document intf))) filename))))


(defun file-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "New"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Open"
                                                   :callback 'default-menu-selected :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Close"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save"
                                                   :callback 'save-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save As"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Revert to Saved"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Empty Trash"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Page Setup"
                                                   :callback 'default-menu-selected :callback-type :interface)
                               (make-instance 'capi:menu-item :text "Print"
                                              :callback 'default-menu-selected :callback-type :interface)))))

(defun edit-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Undo"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Redo"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Cut"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Copy"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Paste"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Delete"
                                                   :callback 'default-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Select All"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Rename Column"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))))

(defun windows-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Minimize"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Zoom"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Bring All To Front"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))))

(defun help-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Delectus Help"
                                                   :callback 'default-menu-selected
                                                   :callback-type :interface)))))

