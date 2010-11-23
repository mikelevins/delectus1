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

(defun current-store-format-version () $delectus-format-1.9a)

(defparameter $delectus-format-sentinel #(68 69 76 67))

;;; ---------------------------------------------------------------------
;;; serialization tags
;;; ---------------------------------------------------------------------

(defparameter $tag-bool 0)
(defparameter $tag-int 1)
(defparameter $tag-string 2)
(defparameter $tag-row 3)
(defparameter $tag-column 4)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(defmethod write-byte-on-vector ((byte integer) (outv vector))
  (assert (<= 0 byte 255)() "Byte value (~S) out of range" byte)
  (assert (array-has-fill-pointer-p outv)() "Output vector lacks a fill-pointer")
  (vector-push-extend byte outv))

(defun write-vector-on-vector (inv outv)
  (assert (array-has-fill-pointer-p outv)() "Output vector lacks a fill-pointer")
  (if (> (length inv) (- (array-dimension outv 0)(fill-pointer outv)))
      (progn
        (adjust-array outv (* 2 (array-dimension outv 0)) :element-type '(unsigned-byte 8) :initial-element 0)
        (write-vector-on-vector inv outv))
      (let* ((start (fill-pointer outv))
             (end (+ start (length inv))))
        (setf (fill-pointer outv) end)
        (replace outv inv :start1 start :end1 end))))

;;; ---------------------------------------------------------------------
;;; serialization output buffer
;;; ---------------------------------------------------------------------

(defun make-serialization-buffer (len)
  (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(defun clear-serialization-buffer (buf)
  (setf (fill-pointer buf) 0)
  buf)

(defmethod write-to-serialization-buffer ((vec vector)(out vector))
  (write-vector-on-vector vec out))

;;; ---------------------------------------------------------------------
;;; serialize-*-to-buffer
;;; ---------------------------------------------------------------------

(defun serialize-delectus-format-to-buffer (out)
  (write-vector-on-vector $delectus-format-sentinel out))

(defun serialize-delectus-format-version-to-buffer (out)
  (write-byte-on-vector (current-store-format-version) out))

(defmethod serialize-boolean-to-buffer ((b (eql t)) out)
  (write-byte-on-vector $tag-bool out)
  (write-byte-on-vector 1 out)
  out)

(defmethod serialize-boolean-to-buffer ((b (eql nil)) out)
  (write-byte-on-vector $tag-bool out)
  (write-byte-on-vector 0 out)
  out)

(defun serialize-count-to-buffer (i out)
  (assert (<= 0 i #xffffffff)() "Count ~A is out of range" i)
  (write-byte-on-vector $tag-int out)
  (write-byte-on-vector (ldb (byte 8 24) i) out)
  (write-byte-on-vector (ldb (byte 8 16) i) out)
  (write-byte-on-vector (ldb (byte 8 8) i) out)
  (write-byte-on-vector (ldb (byte 8 0) i) out)
  out)

(defun serialize-index-to-buffer (i out)
  (assert (<= 0 i #xffffffff)() "Index ~A is out of range" i)
  (write-byte-on-vector $tag-int out)
  (write-byte-on-vector (ldb (byte 8 24) i) out)
  (write-byte-on-vector (ldb (byte 8 16) i) out)
  (write-byte-on-vector (ldb (byte 8 8) i) out)
  (write-byte-on-vector (ldb (byte 8 0) i) out)
  out)

(defun serialize-string-to-buffer (s out)
  (write-byte-on-vector $tag-string out)
  (serialize-count-to-buffer (length s) out)
  (loop for i from 0 upto (1- (length s))
     do (write-byte-on-vector (char-code (aref s i)) out))
  out)

(defun serialize-column-to-buffer (col out)
  (let* ((tag $tag-column)
         (label (label col))
         (index (or (index col) 0))
         (deleted? (deleted? col)))
    (write-byte-on-vector tag out)
    (serialize-string-to-buffer label out)
    (serialize-index-to-buffer index out)
    (serialize-boolean-to-buffer deleted? out))
  out)

(defun serialize-columns-to-buffer (cols out)
  (loop for col across (elements cols)
       do (serialize-column-to-buffer col out))
  out)

(defun serialize-row-to-buffer (row out)
  (let* ((tag $tag-row))
    (write-byte-on-vector tag out)
    (serialize-count-to-buffer (count-elements row) out)
    (loop for el across (elements row)
       do (serialize-string-to-buffer el out)))
  out)

(defun serialize-rows-to-buffer (rows out)
  (loop for row across (elements rows)
     do (serialize-row-to-buffer row out))
  out)

;;; ---------------------------------------------------------------------
;;; deserialize-*-from-buffer
;;; ---------------------------------------------------------------------

(defun deserialize-delectus-format-from-buffer (inbuf pos)
  (values (subseq inbuf pos (+ pos 4))
          (+ pos 4)))

(defun deserialize-delectus-format-version-from-buffer (inbuf pos)
  (values (aref inbuf pos) (1+ pos)))

(defmethod deserialize-boolean-from-buffer (inbuf pos)
  (values (ecase (aref inbuf (1+ pos))
            ((0) nil)
            ((1) t))
          (+ pos 2)))

(defun deserialize-count-from-buffer (inbuf pos)
  (let ((count 0))
    (setf (ldb (byte 8 24) count)(aref inbuf (+ pos 1)))
    (setf (ldb (byte 8 16) count)(aref inbuf (+ pos 2)))
    (setf (ldb (byte 8 8) count)(aref inbuf (+ pos 3)))
    (setf (ldb (byte 8 0) count)(aref inbuf (+ pos 4)))
    (values count (+ pos 5))))

(defun deserialize-index-from-buffer (inbuf pos)
  (let ((count 0))
    (setf (ldb (byte 8 24) count)(aref inbuf (+ pos 1)))
    (setf (ldb (byte 8 16) count)(aref inbuf (+ pos 2)))
    (setf (ldb (byte 8 8) count)(aref inbuf (+ pos 3)))
    (setf (ldb (byte 8 0) count)(aref inbuf (+ pos 4)))
    (values count (+ pos 5))))

(defun deserialize-string-from-buffer (inbuf pos)
  (multiple-value-bind (strlen new-pos)
      (deserialize-count-from-buffer inbuf (1+ pos))
    (let* ((str-bytes (subseq inbuf new-pos (+ new-pos strlen)))
           (str (make-string strlen)))
      (loop for i from 0 upto (1- strlen)
           do (setf (aref str i)(code-char (aref str-bytes i))))
      (values str (+ new-pos strlen)))))

(defun deserialize-column-from-buffer (inbuf pos)
  (multiple-value-bind (label new-pos)
      (deserialize-string-from-buffer inbuf (1+ pos))
    (multiple-value-bind (index new-pos2)
        (deserialize-index-from-buffer inbuf new-pos)
      (multiple-value-bind (deleted? new-pos3)
          (deserialize-boolean-from-buffer inbuf new-pos2)
        (values (make-instance 'column
                               :label label
                               :index index
                               :deleted deleted?)
                new-pos3)))))

(defun deserialize-row-from-buffer (inbuf pos)
  (multiple-value-bind (elt-count new-pos)
      (deserialize-count-from-buffer inbuf (1+ pos))
    (let ((elts nil)
          (remaining-count elt-count)
          (next-pos new-pos))
      (block reading
        (loop
           (when (<= remaining-count 0)
             (return-from reading
               (values (make-row (reverse elts))
                       next-pos)))
           (multiple-value-bind (el new-pos2)
               (deserialize-string-from-buffer inbuf next-pos)
             (setf elts (cons el elts))
             (decf remaining-count)
             (setf next-pos new-pos2)))))))


;;; ---------------------------------------------------------------------
;;; to-serialized-form
;;; ---------------------------------------------------------------------

(defmethod guess-serialized-length ((m model))
  (+ (* 64 (count-elements (columns m)))
     (* 64 (count-elements (rows m)))))

(defmethod to-serialized-form ((m model))
  (let ((outbuf (make-serialization-buffer (guess-serialized-length m))))
    (serialize-delectus-format-to-buffer outbuf)
    (serialize-delectus-format-version-to-buffer outbuf)
    (serialize-columns-to-buffer (columns m) outbuf)
    (serialize-rows-to-buffer (rows m) outbuf)
    outbuf))

;;; ---------------------------------------------------------------------
;;; from-serialized-form
;;; ---------------------------------------------------------------------

(defmethod read-serialized-data ((inbuf array)(tag (eql $tag-row))(index integer))
  (deserialize-row-from-buffer inbuf index))

(defmethod read-serialized-data ((inbuf array)(tag (eql $tag-column))(index integer))
  (deserialize-column-from-buffer inbuf index))

(defmethod read-serialized-data ((inbuf array)(tag (eql $tag-string))(index integer))
  (deserialize-string-from-buffer inbuf index))

(defmethod read-serialized-data ((inbuf array)(tag (eql $tag-int))(index integer))
  (deserialize-index-from-buffer inbuf index))

(defmethod read-serialized-data ((inbuf array)(tag (eql $tag-bool))(index integer))
  (deserialize-boolean-from-buffer inbuf index))

(defmethod from-serialized-form ((inbuf array))
  (let ((format-sentinel-bytes (subseq inbuf 0 4))
        (format-version-byte (aref inbuf 4))
        (pos 5)
        (parts nil))
    (block reading 
      (loop
         (when (>= pos (length inbuf))
           (return-from reading))
         (multiple-value-bind (part new-pos)
             (read-serialized-data inbuf (elt inbuf pos) pos)
           (setf parts (cons part parts))
           (setf pos new-pos))))
    (let ((columns (as 'list (seq:filter #'column? parts)))
          (rows (as 'list (seq:filter #'row? parts))))
      (make-model :columns columns
                  :rows rows))))

;;; ---------------------------------------------------------------------
;;; storing  models
;;; ---------------------------------------------------------------------

(defmethod store-data ((data vector)(path pathname))
  (with-open-file (out path :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-sequence data out)))

(defmethod store ((m model)(path pathname))
  (store-data (to-serialized-form m) path))

(defmethod store ((m model)(path string))
  (store m (pathname path)))

;;; ---------------------------------------------------------------------
;;; loading  models
;;; ---------------------------------------------------------------------

(defmethod load-data ((path pathname))
  (with-open-file (in path :direction :input
                       :element-type '(unsigned-byte 8))
    (let ((inbuf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence inbuf in)
      inbuf)))

(defmethod load-model ((path pathname))
  (from-serialized-form (load-data path)))

(defmethod load-model ((path string))
  (load-model (pathname path)))

