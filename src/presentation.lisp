(in-package :delectus)

(defclass presentation ()
  ((model :reader model :initarg :model :initform (make-instance 'model))
   (changed? :accessor changed? :initform t)
   (sort-column :accessor sort-column :initform nil)
   (reverse-sort? :accessor reverse-sort? :initform nil)
   (filter-string :accessor filter-string :initform nil)
   (deleted-columns :accessor deleted-columns :initarg :deleted-columns :initform (seq:make))
   (deleted-rows :accessor deleted-rows :initarg :deleted-rows :initform (seq:make))
   (present-deleted? :accessor present-deleted? :initarg :present-deleted? :initform nil)
   (presented-columns :accessor presented-columns :initarg :presented-columns :initform (seq:make))
   (presented-rows :accessor presented-rows :initarg :presented-rows :initform (seq:make))))

(defmethod remove-deleted-columns ((pres presentation))
  (seq:difference (columns (model pres))
                  (deleted-columns pres)))

(defmethod remove-deleted-rows ((pres presentation))
  (seq:difference (rows (model pres))
                  (deleted-rows pres)))

(defmethod match-text ((text string) val)
  nil)

(defmethod match-text ((text string)(val string))
  (search text val :test #'equalp))

(defmethod find-text ((text string) row)
  (seq:contains? (elements row) text :test #'match-text))

(defmethod filter-rows ((pres presentation) rows)
  (if (filter-string pres)
      (seq:filter (fun:partial #'find-text (filter-string pres))
                  rows)
      rows))

(defmethod order< ((v1 integer)(v2 integer))
  (< v1 v2))

(defmethod order< ((v1 string)(v2 string))
  (string< v1 v2))

(defmethod order-rows ((column-index integer)(row1 row)(row2 row))
  (order< (row-element row1 column-index)
          (row-element row2 column-index)))

(defmethod sort-rows ((pres presentation) rows &key (reverse nil))
  (if (sort-column pres)
      (let ((new-rows (seq:sort (fun:partial #'order-rows (index (sort-column pres)))
                                rows)))
        (if reverse (seq:reverse new-rows) new-rows))
      rows))

(defmethod update ((pres presentation))
  (when (changed? pres)
    (setf (presented-columns pres)
          (remove-deleted-columns pres))
    (setf (presented-rows pres) 
          (sort-rows pres (filter-rows pres (remove-deleted-rows pres))
                     :reverse (reverse-sort? pres))))
  (setf (changed? pres) nil))

(defmethod count-rows ((pres presentation))
  (update pres)
  (seq:length (presented-rows pres)))

(defmethod count-columns ((pres presentation))
  (update pres)
  (seq:length (presented-columns pres)))

(defmethod value-at ((pres presentation)(column-name string)(row integer))
  (update pres)
  (row-element (seq:element (presented-rows pres) row) (index (find-column (model pres) column-name))))

(defmethod put-value-at ((pres presentation)(column-name string)(row-index integer) val)
  (update pres)
  (set-row-element! (seq:element (presented-rows pres) row-index)
                    (index (find-column (model pres) column-name)) val)
  (setf (changed? pres) t))

(defmethod find-column ((pres presentation)(column-name string))
  (update pres)
  (seq:find (^ (col)(equalp column-name (label col))) 
            (presented-columns pres)))

(defmethod add-column ((pres presentation)(column string))
  (update pres)
  (add-column (model pres) column)
  (setf (changed? pres) t))

(defmethod add-row ((pres presentation))
  (clear-order-function! pres)
  (clear-filter-function pres)
  (update pres)
  (add-row (model pres))
  (setf (changed? pres) t))

(defmethod set-sort-column! ((pres presentation)(column-name string) &key (reverse nil))
  (setf (sort-column pres)(find-column pres column-name))
  (setf (reverse-sort? pres) reverse)
  (setf (changed? pres) t))

(defmethod delete-column! ((pres presentation)(column-name string))
  (let ((found-column (find-column (model pres) column-name)))
    (assert found-column () "No such column: '~A'" column-name)
    (setf (deleted-columns pres)(seq:add-last (deleted-columns pres) found-column))
    (setf (changed? pres) t)))

(defmethod delete-row! ((pres presentation)(row-index integer))
  (let ((found-row (seq:element (presented-rows pres) row-index)))
    (assert found-row () "No such row: '~A'" row-index)
    (setf (deleted-rows pres)(seq:add-last (deleted-rows pres) found-row))
    (setf (changed? pres) t)))
