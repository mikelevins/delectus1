(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; target/action implementation for controls
;;; ---------------------------------------------------------------------

(define-objc-method ("addRow:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (display-message "addRow: called"))

(define-objc-method ("deleteRow:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (display-message "deleteRow: called"))

(define-objc-method ("addColumn:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (let* ((ctrl-desc (find-control (tag sender)))
         (intf (map:get ctrl-desc :interface)))
    (with-dialog-results (str okp)
        (prompt-for-string "Name the new column:" :popup-args `(:owner ,intf))
      (when okp (add-table-column intf (row-pane intf) str)))))

(define-objc-method ("deleteColumn:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (display-message "deleteColumn: called"))

(define-objc-method ("toggleTrash:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (display-message "toggleTrash: called"))

