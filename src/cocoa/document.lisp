(in-package :delectus)

;;; ---------------------------------------------------------------------
;;;  document
;;; ---------------------------------------------------------------------

(define-objc-protocol "NSTableViewDataSource"
    :instance-methods (("numberOfRowsInTableView:" (:unsigned :long)
                                                   objc-object-pointer)
                       ("tableView:objectValueForTableColumn:row:" objc-object-pointer
                                                                   objc-object-pointer
                                                                   (:unsigned :long))
                       ("tableView:setObjectValue:forTableColumn:row:" :void
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       (:unsigned :long))))

(define-objc-class document ()
  ((name :reader name :initarg :name :initform nil)
   (changed? :accessor changed? :initform t)
   (pathname :accessor pathname :initarg :pathname :initform nil)
   (window :reader window :initarg :window :initform nil)
   (presentation :reader presentation :initarg :presentation :initform (make-instance 'presentation)))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(defmethod initialize-instance :after ((doc document) &rest initargs &key (name "Untitled") (presentation nil)
                                       &allow-other-keys)
  (let ((pres (or presentation (make-instance 'presentation))))
    (setf (slot-value doc 'name)  name)
    (setf (slot-value doc 'window)(make-instance 'delectus-window :document doc))))

(defmethod show ((doc document))
  (display (window doc)))

;;; ---------------------------------------------------------------------
;;;  model methods

(defmethod count-rows ((doc document))
  (count-rows (presentation doc)))

(defmethod value-at ((doc document)(column-name string)(row integer))
  (value-at (presentation doc) column-name row))

(defmethod put-value-at! ((doc document)(column-name string)(row integer) val)
  (put-value-at! (presentation doc) column-name row val))

(defmethod add-row! ((doc document))
  (clear-sort! (presentation doc))
  (clear-filter! (presentation doc))
  (add-row! (presentation doc))
  (notify-document-changed! (app) doc))

(defmethod delete-selected-row! ((doc document))
  (let ((row (get-selected-row doc)))
    (mark-row-deleted! row)
    (notify-document-changed! (app) doc)))

(defmethod add-column! ((doc document))
  (with-validated-label-from-user (label)
    (clear-sort! (presentation doc))
    (clear-filter! (presentation doc))
    (add-column! (presentation doc) label)
    (notify-document-changed! (app) doc)))

(defmethod delete-selected-column! ((doc document))
  (let ((col (get-selected-column doc)))
    (mark-column-deleted! col)
    (notify-document-changed! (app) doc)))

(defmethod toggle-trash ((doc document))
  (setf (show-deleted? doc)
        (not (show-deleted? doc)))
  (notify-document-changed! (app) doc))

;;; ---------------------------------------------------------------------
;;;  NSDataSource methods

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self document)
     (table-view objc-object-pointer))
  (count-rows self))

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self document)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (let* ((col-name (invoke-into 'string (invoke column "headerCell") "stringValue"))
         (val (value-at self col-name row)))
    (if (stringp val)
        val
        (princ-to-string val))))

(define-objc-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((self document)
     (table-view objc-object-pointer)
     (object-value objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (let* ((col-name (invoke-into 'string (invoke column "headerCell") "stringValue"))
         (model-value (invoke-into 'string (invoke "NSString" "alloc") "initWithString:" object-value)))
    (put-value-at! self col-name row model-value)))

;;; ---------------------------------------------------------------------
;;;  target/action methods

(define-objc-method ("addRow:" :void)
    ((self document)
     (sender objc-object-pointer))
  (add-row! self))

(define-objc-method ("deleteRow:" :void)
    ((self document)
     (sender objc-object-pointer))
  (delete-selected-row! self))

(define-objc-method ("addColumn:" :void)
    ((self document)
     (sender objc-object-pointer))
  (add-column! self))

(define-objc-method ("deleteColumn:" :void)
    ((self document)
     (sender objc-object-pointer))
  (delete-selected-column! self))

(define-objc-method ("toggleTrash:" :void)
    ((self document)
     (sender objc-object-pointer))
  (toggle-trash self))