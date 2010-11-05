(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; model serialization/deserialization
;;; ---------------------------------------------------------------------

(defclass deserializer ()
  ((input-queue :accessor input-queue :initform nil))
  (:metaclass singleton-class))

(defun deserializer ()(make-instance 'deserializer))

(defun push-deserialized-model (data)
  (setf (input-queue (deserializer))
        (cons data (input-queue (deserializer)))))

(defun pop-deserialized-model (data)
  (let ((data (car (input-queue (deserializer)))))
    (setf (input-queue (deserializer))
          (cdr (input-queue (deserializer))))
    data))

(defmethod to-serialized-form ((row row))
  (vector (deleted? row)
          (as 'vector (seq:image (^ (el) (box:get el))
                                 (elements row)))))

(defmethod to-serialized-form ((col column))
  (vector (deleted? col)(label col)))

(defun deserialize-row (data)
  (let ((deleted? (elt data 0))
        (elements (as 'fset:seq (seq:image #'box:make (elt data 1)))))
    (make-instance 'row :deleted deleted? :elements elements)))

(defun deserialize-column (data)
  (let ((deleted? (elt data 0))
        (label (elt data 1)))
    (make-instance 'column :deleted deleted? :label label)))

(defun deserialized-data->model (model-data)
  (let* ((col-vector (elt model-data 0))
         (cols (as 'fset:seq (seq:image #'deserialize-column col-vector)))
         (rows-vector (elt model-data 1))
         (rows (as 'fset:seq (seq:image #'deserialize-row rows-vector))))
    (make-instance 'model :rows rows :columns cols)))

(defmethod to-serialized-form ((m model) )
  (vector (as 'vector (seq:image #'to-serialized-form (columns m)))
          (as 'vector (seq:image #'to-serialized-form (rows m)))))

(defmethod save-model ((m model)(path pathname))
  (let ((sf `(delectus::push-deserialized-model ,(delectus::to-serialized-form m))))
    (let ((sys::*binary-file-types* 
           (cons "delectus" sys::*binary-file-types*)))
      (hcl:with-output-to-fasl-file (out path)
        (hcl:dump-form sf out)))))

(defmethod save-model ((m model)(path string))
  (serialize m (pathname path)))

(defmethod load-model ((path pathname))
  (let ((sys::*binary-file-types* 
         (cons "delectus" sys::*binary-file-types*)))
    (sys:load-data-file path)
    (let ((model-data (pop-deserialized-model (deserializer))))
      (deserialized-data->model model-data))))

(defmethod load-model ((path string))
  (deserialize-model (pathname path)))

