(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; rows
;;; ---------------------------------------------------------------------

(defclass row ()
  ((deleted? :accessor deleted? :initform nil)
   (elements :accessor elements :initarg :elements)))

(defun row (&rest elts)
  (make-instance 'row :elements (seq:image #'box:make elts)))

(defmethod element ((row row)(index integer))
  (box:get (seq:element (elements row) index)))

(defmethod set-element! ((row row)(index integer) val)
  (box:put val (seq:element (elements row) index)))

(defsetf element set-element!)

(defmethod add-element ((row row) &optional initial-value)
  (setf (elements row)
        (add-last (elements row)
                  (box:make initial-value))))

;;; (setf $row (row :zero :one :two :three))
;;; (element $row 2)
;;; (setf (element $row 2) "Two")

(defmethod row-contains? ((row row) val)
  nil)

(defmethod row-contains? ((row row) (val integer))
  (seq:contains? (elements row) val :test (^ (b v)(= v (box:get b)))))

(defmethod row-contains? ((row row) (val string))
  (seq:contains? (elements row) val
                 :test (^ (b v) (search v (box:get b) :test #'equalp))))

;;; ---------------------------------------------------------------------
;;; columns
;;; ---------------------------------------------------------------------

(defclass column ()
  ((deleted? :accessor deleted? :initform nil)
   (label :accessor label :initarg :label)))

(defun column (label)
  (make-instance 'column :label label))

;;; ---------------------------------------------------------------------
;;; models
;;; ---------------------------------------------------------------------

(defclass model ()
  ((columns :accessor columns :initform (seq:make) :initarg :columns)
   (name->index-table :reader name->index-table :initform (make-hash-table :test 'equalp))
   (index->name-table :reader index->name-table :initform (make-hash-table :test 'equalp))
   (rows :accessor rows :initform (seq:make) :initarg :rows)))

(defmethod initialize-instance :after ((m model) &rest initarg &key &allow-other-keys)
  (let ((indexes (seq:range 0 (seq:length (columns m)))))
    (seq:image (^ (i)
                 (setf (gethash (label (seq:element (columns m) i)) (name->index-table m))
                       i)
                 (setf (gethash i (index->name-table m))
                       (label (seq:element (columns m) i))))
               indexes)))

(defmethod count-columns ((m model))
  (seq:length (columns m)))

(defmethod count-rows ((m model))
  (seq:length (rows m)))

(defmethod column-index ((m model)(column-name string))
  (gethash column-name (name->index-table m) nil))

(defmethod column-index ((m model)(column column))
  (column-index m (label column)))

(defmethod column-name ((m model)(column-index integer))
  (gethash column-index (index->name-table m) nil))

(defmethod column-name ((m model)(column column))
  (label column))

(defmethod find-column ((m model)(column-name string))
  (seq:find (^ (col)(equalp column-name (label col))) (columns m)))

(defmethod value-at ((m model)(column-name string)(row-index integer))
  (element (seq:element (rows m) row-index)
           (column-index m column-name)))

(defmethod value-at ((m model)(column column)(row-index integer))
  (value-at m (column-name column) row-index))

(defmethod put-value-at! ((m model)(column-name string)(row-index integer) val)
  (setf (element (seq:element (rows m) row-index)
                 (column-index m column-name))
        val))

(defmethod put-value-at! ((m model)(column column)(row-index integer) val)
  (set-value-at! m (column-name column) row-index val))

(defmethod add-column! ((m model)(label string))
  (let ((next-index (count-columns m)))
    (setf (columns m)(add-last (columns m)(column label)))
    (setf (gethash label (name->index-table m)) next-index)
    (setf (gethash next-index (index->name-table m)) label)
    (seq:image (^ (row)(add-element row nil)) (rows m))))

(defmethod add-row! ((m model))
  (setf (rows m)
        (seq:add-last (rows m)
                      (apply #'row (seq:repeat (count-columns m) nil)))))

(defun model (&key rows columns)
  (let ((col-count (seq:length columns)))
    (assert (every (^ (row)(= (seq:length row) col-count))
                   rows)
            () "Malformed model data")
    (make-instance 'model
                   :columns (apply #'seq:make (seq:image #'column columns))
                   :rows (seq:image (^ (row)(apply #'row row)) rows))))

