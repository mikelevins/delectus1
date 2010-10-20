(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus data model
;;; ---------------------------------------------------------------------

(defclass model ()
  ((column-name->index :accessor column-name->index :initform (map:make))
   (index->column-name :accessor index->column-name :initform (map:make))
   (columns :accessor columns :initarg :columns :initform (seq:make))
   (rows :accessor rows :initarg :rows :initform (seq:make))))

(defmethod initialize-instance :before ((m model) &rest initargs &key (columns nil)(rows nil)
                                       &allow-other-keys)
  (assert (and (= (length columns)
                  (length (first rows)))
               (every (lambda (r)(= (length r)(length columns)))
                      rows))
          ()
          "Malformed row and column data"))

(defmethod initialize-instance :after ((m model) &rest initargs &key (columns nil)(rows nil)
                                       &allow-other-keys)
  (when (or columns rows)
    (let* ((column-names (or columns (take-letters (length (first rows))))))
      (setf (columns m)(as 'fset:seq (seq:image 'box:make column-names)))
      (setf (rows m)(as 'fset:seq 
                        (seq:image (^ (row)(as 'fset:seq (seq:image 'box:make row))) 
                                   rows))))))

(defmethod count-rows ((m model))
  (seq:length (rows m)))

(defmethod count-columns ((m model))
  (seq:length (columns m)))

(defmethod column-index ((m model)(column-name string))
  (seq:position (^ (col)(equalp col column-name)) (columns m)))

(defmethod value-at ((m model)(column-name string)(row integer))
  (let* ((col (column-index m column-name))
         (box (seq:element (seq:element (rows m) row) col)))
    (box:get box)))

(defmethod put-value-at ((m model)(column-name string)(row-index integer) val)
  (let* ((col (column-index m column-name))
         (box (seq:element (seq:element (rows m) row-index) col)))
    (box:put val box)))

(defmethod add-column ((m model)(column string))
  (let ((col-pos (column-index m column)))
    (if col-pos
        (error "Column '~A' already exists" column)
        (let* ((last-column-index (column-index m (seq:last (columns m))))
               (next-column-index (1+ last-column-index)))
          (setf (columns m)(seq:add-last (columns m) column))
          (setf (column-name->index m)
                (map:associate (column-name->index m) column next-column-index))
          (setf (index->column-name m)
                (map:associate (index->column-name m) next-column-index column))
          (setf (rows m)
                (seq:image (^ (row)(seq:add-last row nil))
                           (rows m)))))))

(defmethod add-row ((m model) &optional before)
  (let ((row-count (count-rows m))
        (before (or before row-count))
        (old-rows (rows m))
        (new-row (apply 'seq:make (as 'list (seq:repeat (count-columns m) (box:make nil))))))
    (if (> before row-count)
        (error "Tried to insert a row too far past the last row.")
        (cond
          ((zerop before)
           (setf (rows m)(seq:add-first new-row old-rows)))
          ((= before row-count)
           (setf (rows m)(seq:add-last old-rows new-row)))
          (t (setf (rows m)
                   (seq:concat (seq:subsequence old-rows 0 before)
                               (seq:add-first new-row
                                              (seq:subsequence old-rows before)))))))))

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
