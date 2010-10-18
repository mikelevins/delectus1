(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus data model
;;; ---------------------------------------------------------------------

(defun validate-model-contents (contents)
  (let ((col-count (length (first contents)))
        (row-count (length contents)))
    (assert (every (^ (c)(= col-count (length c))) contents)()
            "Malformed model contents (rows are different lengths)")
    (values contents col-count row-count)))

(defun make-model (&optional contents)
  (if contents
      (multiple-value-bind (contents column-count row-count)(validate-model-contents contents)
        (make-array (list row-count column-count) :adjustable t :initial-contents contents))
      (make-array '(0 0) :adjustable t)))

(defmethod count-rows ((model array))
  (first (array-dimensions model)))

(defmethod count-columns ((model array))
  (second (array-dimensions model)))

(defmethod column-index ((model array)(column-name string))
  (block searching
    (dotimes (i (count-columns model))
      (when (equalp column-name (aref model 0 i))
        (return-from searching i)))
    nil))

(defmethod columns ((model array))
  (loop for i from 0 upto (1- (count-columns model))
       do collect (aref model 0 i)))

(defmethod rows ((model array))
  (loop for i from 0 upto (1- (count-rows model))
       do collect i))

(defmethod value-at ((model array)(column string)(row integer))
  (let ((col-pos (column-index model column)))
    (if col-pos
        (aref model row col-pos)
        (error "Column '~A' not found" column))))

(defmethod put-value-at ((model fset:map)(column string)(row integer) val)
  (let ((col-pos (column-index model column)))
    (if col-pos
        (setf (aref model row col-pos) val)
        (error "Column '~A' not found" column))))

(defmethod add-column ((model array)(column string))
  (let ((col-pos (column-index model column)))
    (if col-pos
        (error "Column '~A' already exists" column)
        (let* ((old-dims (array-dimensions model))
               (new-dims (list (first old-dims)(1+ (second old-dims))))
               (new-model (adjust-array model new-dims :initial-element nil)))
          (setf (aref new-model 0 (second old-dims)) column)
          new-model))))

(defmethod add-row ((model array) &optional before)
  (let ((before (or before (count-rows model))))
    (if (> before (count-rows model))
        (error "Tried to insert a row too far past the last row.")
        (let* ((old-dims (array-dimensions model))
               (new-dims (list (1+ (first old-dims))(second old-dims)))
               (new-model (adjust-array model new-dims :initial-element nil)))
          (if (= before (first old-dims))
              new-model
              (progn
                (loop for j from (first old-dims) downto (1+ before)
                     do (loop for i from 0 upto (1- (count-columns model))
                             do (setf (aref model j i)
                                      (aref model (1- j) i))))
                (loop for i from 0 upto (1- (count-columns model))
                             do (setf (aref model before i) nil))
                new-model))))))

;;; ---------------------------------------------------------------------
;;; Presentation
;;; ---------------------------------------------------------------------
;;; Requirements:
;;; 1. Column deletion
;;;   a. mark a column deleted
;;;   b. unmark a column deleted
;;; 2. Row deletion
;;;   a. mark a row deleted
;;;   b. unmark a row deleted
;;; 3. Row filters
;;;   a. presentation must supply filtered row indexes
;;; 4. Row sort
;;;   a. presentation must supply row indexes in sorted order
;;; 5. Show/hide deleted
;;;   a. When hide deleted:
;;;      - don't display deleted columns
;;;      - don't display deleted rows
;;;   b. When show deleted:
;;;      - display deleted columns
;;;      - display deleted rows

(defclass presentation ()
  ((model :reader model :initarg :model)
   (deleted-columns :accessor deleted-columns :initarg :deleted-columns :initform nil)
   (deleted-rows :accessor deleted-rows :initarg :deleted-rows :initform nil)
   (row-filter-fn :accessor row-filter-fn :initarg :row-filter-fn
                  :initform (lambda (row-index) t))
   (row-order-fn :accessor row-order-fn :initarg :row-order-fn
                 :initform (lambda (row-index1 row-index-2) nil))
   (show-deleted? :accessor show-deleted? :initarg :show-deleted? :initform nil)
   (changed? :accessor changed? :initarg :changed? :initform t)
   (column-map :accessor column-map :initarg :column-map)
   (row-map :accessor row-map :initarg :row-map)))

;;; the column map is an ordered-map that maps each
;;; visible column to its index in the underlying array
(defmethod compute-visible-columns ((pres presentation))
  (let* ((visible-columns (if (show-deleted? pres)
                              (columns (model pres))
                              (seq:difference (columns (model pres))
                                              (deleted-columns pres)
                                              :test 'equalp)))
         (visible-indexes (seq:image (^ (vc)(seq:position (^ (c)(equalp c vc))
                                                          (columns (model pres))))
                                     visible-columns)))
    (setf (column-map pres)
          (make-instance 'map:ordered-map 
                         :entries (seq:zip visible-columns visible-indexes)))))

;;; the row map is a vector of integer values. Each
;;; value is the index of the row in the underlying array.
;;; The value at zero will always be zero, because it's
;;; the columns, which never change their position and
;;; are never deleted. All the subsequent values may be
;;; arbitrary integers, though, because sorts may arbitrarily
;;; change the order of rows, and filters and deletion
;;; may remove arbitrary rows.
(defmethod compute-visible-rows ((pres presentation))
  (let* ((all-rows (rows (model pres)))
         (visible-rows (if (show-deleted? pres)
                           all-rows
                           (seq:difference all-rows (deleted-rows pres))))
         (filtered-rows (seq:filter (row-filter-fn pres) visible-rows))
         (sorted-rows (as 'vector (seq:sort (row-order-fn pres) filtered-rows))))
    (setf (row-map pres) sorted-rows)))

(defmethod update-presentation ((pres presentation))
  (when (changed? pres)
    (compute-visible-columns pres)
    (compute-visible-rows pres))
  (setf (changed? pres) nil))

(defmethod count-rows ((pres presentation))
  (update-presentation pres)
  (seq:length (row-map pres)))

(defmethod count-columns ((pres presentation))
  (update-presentation pres)
  (seq:length (map:keys (column-map pres))))

(defmethod columns ((pres presentation))
  (update-presentation pres)
  (map:keys (column-map pres)))

(defmethod rows ((pres presentation))
  (update-presentation pres)
  (seq:range 1 (count-rows pres)))

(defmethod value-at ((pres presentation)(column string)(row integer))
  (update-presentation pres)
  (let ((i (map:get (column-map pres) column :test #'equalp))
        (j (elt (row-map pres) row)))
    (aref (model pres) j i)))

(defmethod put-value-at ((pres presentation)(column string)(row integer) val)
  (update-presentation pres)
  (let ((i (map:get (column-map pres) column :test #'equalp))
        (j (elt (row-map pres) row)))
    (setf (aref (model pres) j i) val)))

(defmethod add-column ((pres presentation)(column string))
  (add-column (model pres) column)
  (setf (changed? pres) t))

(defmethod add-row ((pres presentation) &optional before)
  (let ((j (elt (row-map pres) row)))
    (add-row (model pres) j)
    (setf (changed? pres) t)))

;;; ---------------------------------------------------------------------
;;; NSDataSource API
;;; ---------------------------------------------------------------------

(define-objc-protocol "NSTableViewDataSource"
    :instance-methods (("numberOfRowsInTableView:" (:unsigned :long)
                                                   objc-object-pointer)
                       ("tableView:objectValueForTableColumn:row:" objc-object-pointer
                                                                   objc-object-pointer
                                                                   objc-object-pointer
                                                                   (:unsigned :long))
                       ("tableView:setObjectValue:forTableColumn:row:" :void
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       (:unsigned :long))))

(define-objc-class data-source ()
  ((model :reader model :initarg :model :initform (map:make)))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self data-source)
     (table-view objc-object-pointer))
  (count-rows (model self)))

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self data-source)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (value-at (model self) (invoke (invoke column "headerCell") "textValue") row))

(define-objc-method ("tableView:setObjectValue:forTableColumn:row:" objc-object-pointer)
    ((self data-source)
     (table-view objc-object-pointer)
     (object-value objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (put-value-at (model self)
                (invoke (invoke column "headerCell") "textValue") row
                (invoke-into 'string object-value "description")))
