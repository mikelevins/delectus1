(in-package :delectus)

(defparameter *app-delegate* nil)

(define-objc-class app-delegate ()
  ((widget->interface-table :accessor widget->interface-table :initform (map:make)))
  (:objc-class-name "AppDelegate"))


;;; ---------------------------------------------------------------------
;;; widget tracking
;;; ---------------------------------------------------------------------

(let ((next-id 0))
  (defun next-widget-id ()
    (let ((id next-id))
      (setf next-id (1+ next-id))
      id)))

(defmethod register-tag->interface (tagnum intf)
  (setf (widget->interface-table (app-delegate))
        (map:merge (widget->interface-table (app-delegate))
                   (map:make tagnum intf))))

(defun tag->interface (tagnum)
  (map:get (widget->interface-table (app-delegate)) tagnum :default nil))

;;; ---------------------------------------------------------------------
;;; target/action implementation for controls
;;; ---------------------------------------------------------------------

(define-objc-method ("addRow:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  )

(define-objc-method ("deleteRow:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (contain (make-instance 'title-pane :text "deleteRow: called")))

(define-objc-method ("addColumn:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (let* ((intf (widget->interface sender)))
    (with-dialog-results (str okp)
        (prompt-for-string "Name the new column:" :popup-args `(:owner ,intf))
      (when okp (add-table-column intf (row-pane intf) str)))))

(define-objc-method ("deleteColumn:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (contain (make-instance 'title-pane :text "deleteColumn: called")))

(define-objc-method ("toggleTrash:" :void)
    ((self app-delegate)
     (sender objc-object-pointer))
  (contain (make-instance 'title-pane :text "toggleTrash: called")))

;;; ---------------------------------------------------------------------
;;; fetching the delegate
;;; ---------------------------------------------------------------------

(defun app-delegate ()
  (unless *app-delegate*
    (setf *app-delegate* (make-instance 'app-delegate)))
  *app-delegate*)

;;; (app-delegate)