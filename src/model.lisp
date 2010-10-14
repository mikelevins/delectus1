(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus data
;;; ---------------------------------------------------------------------
;;; an fset:seq of fset:seqs
;;; the head of the seq is a seq of column headers
;;; the tail of the seq is the sequence of rows
;;; each row is the same length as the seq of headers

(define-condition model-error ()())
(define-condition column-exists-error (model-error)
  ((column-name :reader column-name :initarg :column-name))
  (:report (lambda (condition stream)
             (format stream "Column '~A' already exists" 
                     (column-name condition)))))

(defmethod columns ((d fset:seq))
  (seq:head d))

(defmethod rows ((d fset:seq))
  (seq:tail d))

(defmethod add-column ((d fset:seq)(name string))
  (when (seq:find (^ (c)(equalp name c))
                  (columns d))
    (error 'column-exists-error :column-name name))
  (seq:add-first (seq:add-last (columns d) name)
                 (seq:image (^ (s)(seq:add-last s nil))
                            (rows d))))

;;; ---------------------------------------------------------------------
;;; delectus model
;;; ---------------------------------------------------------------------

(defun default-filter (x) t)
(defun default-compare (u v) nil)

(defclass delectus-model ()
  ((changed? :accessor changed? :initform t)
   (deleted-columns :accessor deleted-columns :initform nil)
   (deleted-rows :accessor deleted-rows :initform nil)
   ;; the last-read data
   (data :accessor data :initarg :data :initform nil)
   ;; a user-specified function that determines which data to present
   (filter-fn :accessor filter-fn :initarg :filter-fn :initform (function default-filter))
   ;; a user-specified function that determines how to order the data
   (compare-fn :accessor compare-fn :initarg :compare-fn :initform (function default-compare))
   ;; the data that are to be presented in the UI
   (presentation :accessor presentation :initform nil)))

(defmethod (setf deleted-columns) :after (val (m delectus-model))
  (setf (changed? m) t))

(defmethod (setf deleted-rows) :after (val (m delectus-model))
  (setf (changed? m) t))

(defmethod (setf data) :after (val (m delectus-model))
  (setf (changed? m) t))

(defmethod (setf filter-fn) :after (val (m delectus-model))
  (setf (changed? m) t))

(defmethod (setf compare-fn) :after (val (m delectus-model))
  (setf (changed? m) t))

(defun indexes->elements (s indexes)
  (seq:image (fun:partial 'seq:element s) indexes))

(defun indexed-columns-from-rows (rows indexes)
  (seq:image (fun:partial (fun:flip #'indexes->elements) indexes)
             rows))

(defmethod remove-deleted-items ((m delectus-model) mdata)
  (let* ((del-cols (deleted-columns m))
         (col-indexes (seq:range 0 (seq:length (columns (data m)))))
         (live-col-indexes (seq:difference col-indexes del-cols))
         (del-rows (deleted-rows m))
         (row-indexes (seq:range 0 (seq:length (rows (data m)))))
         (live-row-indexes (seq:difference row-indexes del-rows))
         (new-cols (indexes->elements (columns mdata) live-col-indexes))
         (new-rows (indexes->elements (indexed-columns-from-rows (rows mdata) live-col-indexes)
                                      live-row-indexes)))
    (seq:add-first new-cols new-rows)))

(defmethod update-presentation ((m delectus-model))
  (setf (presentation m)
        (seq:sort (compare-fn m)
                  (seq:filter (filter-fn m)
                              (remove-deleted-items m (data me)))))
  (setf (changed? m) nil))

(defmethod presentation :before ((m delectus-model))
  (when (changed? m)
    (update-presentation m)))

(defmethod column-name->index ((m delectus-model)(name string))
  (seq:position (^ (u)(equalp name u))
                (columns (data m))))

;;; (setf $m (make-instance 'delectus-model))
;;; (presentation $m)