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

