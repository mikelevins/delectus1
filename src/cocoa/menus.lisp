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

(defun new-menu-selected (intf)
  (new-untitled-document))

(defun open-menu-selected (intf)
  (multiple-value-bind (filename successp filter-name)
      (prompt-for-file "Open:" :filter "*.delectus" :operation :open)
    (when successp
      (open-document filename))))

(defun close-menu-selected (intf)
  (when (changed? (document intf))
    (save-menu-selected intf))
  (destroy intf))

(defun save-as-menu-selected (intf)
  (multiple-value-bind (filename successp filter-name)
      (prompt-for-file "Save as:" :filter "*.delectus" :operation :save)
    (when successp
      (let* ((doc (document intf))
             (pres (presentation doc))
             (mod (get-model pres)))
        (save-model mod filename)
        (setf (changed? doc) nil)
        (setf (pathname doc) (pathname filename))))))

(defun save-menu-selected (intf)
  (let* ((doc (document intf))
         (pres (presentation doc))
         (mod (get-model pres)))
    (if (pathname doc)
        (progn
          (save-model mod (pathname doc))
          (setf (changed? doc) nil))
        (save-as-menu-selected intf))))

(defun file-menu-items (intf)
  (list (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "New"
                                                   :callback 'new-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Open"
                                                   :callback 'open-menu-selected :callback-type :interface)))
        (make-instance 'menu-component
                       :items (list (make-instance 'capi:menu-item :text "Close"
                                                   :callback 'close-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save"
                                                   :callback 'save-menu-selected :callback-type :interface)
                                    (make-instance 'capi:menu-item :text "Save As"
                                                   :callback 'save-as-menu-selected :callback-type :interface)
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

