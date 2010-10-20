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

