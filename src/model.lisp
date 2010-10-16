(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus data
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

(defmethod column-index ((model array)(column string))
  (block searching
    (dotimes (i (count-columns model))
      (when (equalp column (aref model 0 i))
        (return-from searching i)))
    nil))

(defmethod columns ((model array))
  (loop for i from 0 upto (1- (count-columns model))
       do collect (aref model 0 i)))

(defmethod value-at ((model array)(column string)(row integer))
  (let ((col-pos (column-index model column)))
    (if col-pos
        (aref model (1+ row) col-pos)
        (error "Column '~A' not found" column))))

(defmethod put-value-at ((model fset:map)(column string)(row integer) val)
  (let ((col-pos (column-index model column)))
    (if col-pos
        (setf (aref model (1+ row) col-pos) val)
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
