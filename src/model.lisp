(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus data model
;;; ---------------------------------------------------------------------

(defclass column ()
  ((label :accessor label :initarg :label)
   (index :accessor index :initarg :index)))

(defmethod print-object ((col column)(s stream))
  (print-unreadable-object (col s :type t)
    (format s "~A" (label col))))

(defclass row ()
  ((elements :accessor elements)))

(defmethod initialize-instance :after ((r row) &rest initargs &key (elements nil)
                                       &allow-other-keys)
  (if elements
      (setf (elements r)
            (as 'fset:seq
                (seq:image (^ (e)(box:make e))
                           elements)))
      (setf (elements r)(seq:make))))

(defmethod row-element ((r row)(index integer))
  (box:get (seq:element (elements r) index)))

(defmethod set-row-element! ((r row)(index integer) val)
  (box:put val (seq:element (elements r) index)))

(defclass model ()
  ((columns :accessor columns)
   (rows :accessor rows)))

(defmethod initialize-instance :before ((m model) &rest initargs &key (columns nil)(rows nil)
                                       &allow-other-keys)
  (assert (and (= (seq:length columns)
                  (seq:length (first rows)))
               (seq:every? (lambda (r)(= (seq:length r)(seq:length columns)))
                           rows))
          ()
          "Malformed row and column data"))

(defmethod initialize-instance :after ((m model) &rest initargs &key (columns nil)(rows nil)
                                       &allow-other-keys)
  (when (or columns rows)
    (let* ((column-names (or columns (take-letters (length (first rows))))))
      (setf (columns m)(as 'fset:seq (seq:image (^ (cn i) (make-instance 'column :label cn :index i))
                                                column-names
                                                (seq:range 0 (seq:length column-names)))))
      (setf (rows m)
            (as 'fset:seq 
                (seq:image (^ (r)(make-instance 'row :elements r)) 
                           rows))))))

(defmethod count-rows ((m model))
  (seq:length (rows m)))

(defmethod count-columns ((m model))
  (seq:length (columns m)))

(defmethod find-column ((m model)(nm string))
  (seq:find (^ (col)(equalp nm (label col)))
            (columns m)))

(defmethod value-at ((m model)(column-name string)(row integer))
  (row-element (seq:element (rows m) row) 
               (index (find-column m column-name))))

(defmethod put-value-at ((m model)(column-name string)(row-index integer) val)
  (set-row-element! (seq:element (rows m) row-index) 
                    (index (find-column m column-name))
                    val))

(defmethod add-column ((m model)(column-name string))
  (if (find-column m column-name)
      (error "Column '~A' already exists" column)
      (let* ((last-column (seq:last (columns m)))
             (new-index (1+ (index last-column))))
        (setf (columns m)
              (seq:add-last (columns m)
                            (make-instance 'column :label column-name :index new-index)))
        (dotimes (i (count-rows m))
          (let ((row (seq:element (rows m) i)))
            (setf (elements row)
                  (seq:add-last (elements row)
                                (box:make nil))))))))

(defmethod add-row ((m model))
  (let ((old-rows (rows m))
        (new-row (make-instance 'row :elements (seq:image 'box:make (seq:repeat (count-columns m) nil)))))
    (seq:add-first new-row old-rows)))

;;; row utils

