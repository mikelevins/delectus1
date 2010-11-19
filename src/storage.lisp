;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          storage.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       serialization and storage of the data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)


(defparameter $delectus-format-alpha-1 0)
(defparameter $delectus-format-alpha-2 1)
(defparameter $delectus-format-alpha-4 2)
(defparameter $delectus-format-beta-2 3)
(defparameter $delectus-format-1.9a 4)

(defun current-store-format () $delectus-format-1.9a)

(defmethod to-serialized-form ((col column))
  (vector (label col)
          (index col)
          (deleted? col)))

(defmethod to-serialized-form ((row row))
  (as 'vector (seq:image #'val (elements row))))

(defun columns-to-serialized-form (cols)
  (let* ((col-count (seq:length cols))
         (output (make-array col-count)))
    (loop for col-index from 0 upto (1- col-count)
       do (setf (aref output col-index)
                (to-serialized-form (seq:element cols col-index))))
    output))

(defun rows-to-serialized-form (rows)
  (let* ((row-count (seq:length rows))
         (col-count (seq:length (elements (seq:element rows 0))))
         (output (make-array (list row-count col-count))))
    (loop for row-index from 0 upto (1- row-count)
         do (loop for col-index from 0 upto (1- col-count)
                 do (setf (aref output row-index col-index)
                       (element (seq:element rows row-index) col-index))))
    output))

(defun rows-to-serialized-form (rows)
  (as 'vector
      (seq:image #'to-serialized-form rows)))

(defmethod to-serialized-form ((m model))
  (vector :format :delectus
          (current-store-format)
          (columns-to-serialized-form (columns m))
          (rows-to-serialized-form (rows m))))

(defun from-serialized-form (data)
  )

(defmethod store ((m model)(path pathname))
  (cl-store:store (to-serialized-form m) path))

(defmethod store ((m model)(path string))
  (store m (pathname path)))

(defmethod reload ((path pathname))
  (from-serialized-form (cl-store:restore path)))

(defmethod reload ((path string))
  (reload m (pathname path)))

