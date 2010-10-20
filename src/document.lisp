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
   (model :reader model :initarg :model :initform nil)
   (window :reader window :initarg :window :initform nil)
   (deleted-columns :accessor deleted-columns :initform (seq:make))
   (deleted-rows :accessor deleted-rows :initform (seq:make))
   (filter-fn :accessor filter-fn :initform (lambda (row) t))
   (order-fn :accessor order-fn :initform (lambda (u v) nil))
   (show-deleted? :accessor show-deleted? :initform nil)
   (changed? :accessor changed? :initform t)
   (presented-columns :accessor presented-columns :initform nil)
   (presented-rows :accessor presented-rows :initform nil))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(defmethod initialize-instance :after ((doc document) &rest initargs &key (name "Untitled") (model nil)
                                       &allow-other-keys)
  (let ((m (or model (make-instance 'model))))
    (setf (slot-value doc 'model)  m)
    (setf (slot-value doc 'name)  name)
    (setf (slot-value doc 'window)  (make-instance 'delectus-window :document doc))))

(defmethod show ((doc document))
  (display (window doc)))

(defmethod update-presentation ((doc document))
  (when (changed? doc)
    (let* ((all-columns (columns (model doc)))
           (all-rows (rows (model doc)))
           (visible-columns (if (or (show-deleted? doc)
                                    (seq:empty? (deleted-columns doc)))
                             all-columns
                             (seq:difference all-columns (deleted-columns doc) :test #'equalp)))
           (visible-rows (if (or (show-deleted? doc)
                                 (seq:empty? (deleted-rows doc)))
                             all-rows
                             (seq:difference all-rows
                                             (seq:image (^ (ri)(seq:element all-rows ri))
                                                        (deleted-rows doc))
                                             :test #'equalp)))
           (filtered-rows (seq:filter (filter-fn doc) visible-rows))           
           (sorted-rows (seq:sort (order-fn doc) filtered-rows)))
      (setf (presented-columns doc) visible-columns)
      (setf (presented-rows doc) sorted-rows)))
  (setf (changed? doc) nil))

;;; ---------------------------------------------------------------------
;;;  model methods

(defmethod count-rows ((doc document))
  (update-presentation doc)
  (seq:length (presented-rows doc)))

(defmethod column-index ((doc document)(column-name string))
  (seq:position (^ (col)(equalp col column-name)) (presented-columns doc)))

(defmethod value-at ((doc document)(column-name string)(row integer))
  (update-presentation doc)
  (let* ((col (column-index doc column-name))
         (box (seq:element (seq:element (presented-rows m) row) col)))
    (box:get box)))

(defmethod put-value-at ((doc document)(column-name string)(row integer) val)
  (update-presentation doc)
  (let* ((col (column-index doc column-name))
         (box (seq:element (seq:element (presented-rows m) row) col)))
    (box:put val box)
    (setf (changed? doc) t)))

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
  (value-at self
            column
            (invoke (invoke column "headerCell") "stringValue")
            row))

(define-objc-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((self document)
     (table-view objc-object-pointer)
     (object-value objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (put-value-at self
                column
                (invoke (invoke column "headerCell") "stringValue")
                row
                object-value))
