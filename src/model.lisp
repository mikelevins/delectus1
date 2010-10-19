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
    (let* ((column-names (or columns (take-letters (length (first rows)))))
           (column-indexes (seq:range 0 (seq:length column-names))))
      (setf (column-name->index m)(map:zipmap column-names column-indexes))
      (setf (index->column-name m)(map:zipmap column-indexes column-names))
      (setf (columns m)(as 'fset:seq columns))
      (setf (rows m)(as 'fset:seq 
                        (seq:image (^ (row)(as 'fset:seq row)) 
                                   rows))))))

(defmethod count-rows ((m model))
  (seq:length (rows m)))

(defmethod count-columns ((m model))
  (seq:length (columns m)))

(defmethod column-index ((m model)(column-name string))
  (map:get (column-name->index m) column-name :test #'equalp))

(defmethod value-at ((m model)(column-name string)(row integer))
  (let ((col (column-index m column-name)))
    (seq:element (seq:element (rows m) row) col)))

(defmethod update-value-at ((seq fset:seq) index val)
  (assert (< -1 index (seq:length seq))()
          "Index out of range")
  (seq:concat (seq:subsequence seq 0 index)
              (seq:add-first val (seq:subsequence seq (1+ index)))))

(defmethod put-value-at ((m model)(column-name string)(row-index integer) val)
  (let* ((col-index (column-index m column-name))
         (row (seq:element (rows m) row-index))
         (new-row (update-value-at row col-index val))
         (new-rows (update-value-at (rows m) row-index new-row)))
    (setf (rows m) new-rows)))

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
        (new-row (apply 'seq:make (as 'list (seq:repeat (count-columns m) nil)))))
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
;;;  data source
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



(define-objc-class data-source ()
  ((model :reader model :initarg :model)
   (deleted-columns :accessor deleted-columns :initform (seq:make))
   (deleted-rows :accessor deleted-rows :initform (seq:make))
   (filter-fn :accessor filter-fn :initform (lambda (row) t))
   (order-fn :accessor order-fn :initform (lambda (u v) nil))
   (changed? :accessor changed? :initform t)
   (view :accessor view :initform nil))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(defmethod count-rows ((ds data-source))
  (count-rows (model ds)))

(defmethod value-at ((ds data-source)(column-name string)(row integer))
  (value-at (model ds) column-name row))

(defmethod put-value-at ((ds data-source)(column-name string)(row integer) val)
  (put-value-at (model ds) column-name row val))

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self data-source)
     (table-view objc-object-pointer))
  (count-rows (model self)))

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self data-source)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (value-at self
            column
            (invoke (invoke column "headerCell") "stringValue")
            row))

(define-objc-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((self data-source)
     (table-view objc-object-pointer)
     (object-value objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (put-value-at self
                column
                (invoke (invoke column "headerCell") "stringValue")
                row
                object-value))
