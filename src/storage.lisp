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

(defun sum-vector-lengths (vecs)
  (seq:reduce #'+ (seq:image #'length vecs) :initial-value 0))

(defun write-vector-on (inv outv &key (start 0))
  (replace outv inv :start1 start :end1 (+ start (length inv))))

;;; ---------------------------------------------------------------------
;;; binary field definitions
;;; ---------------------------------------------------------------------

(bt:define-unsigned octet 1)
(bt:define-unsigned int4 4)

;;; ---------------------------------------------------------------------
;;; converting booleans
;;; ---------------------------------------------------------------------

(defmethod bool->serialized-form ((b (eql nil)))
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents `(,$tag-bool 0)))

(defmethod bool->serialized-form ((b (eql t)))
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents `(,$tag-bool 1)))

;;; ---------------------------------------------------------------------
;;; converting numbers
;;; ---------------------------------------------------------------------

(defmethod integer->serialized-form ((i integer))
  (assert (<= 0 i #xffffffff)() "Integer ~A is out of range" i)
  (vector $tag-int 
          (ldb (byte 8 24) i)
          (ldb (byte 8 16) i)
          (ldb (byte 8 8) i)
          (ldb (byte 8 0) i)))

;;; ---------------------------------------------------------------------
;;; converting strings
;;; ---------------------------------------------------------------------

(defmethod string->octets ((s string))
  (let* ((len (length s))
         (outbuf (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (dotimes (i len)
      (vector-push (char-code (elt s i)) outbuf))
    outbuf))

(defmethod octets->string ((os vector))
  (coerce (seq:image #'code-char os) 'string))

(defmethod string->serialized-form ((s string))
  (let* ((tag $tag-string)
         (outs (string->octets s))
         (len (length outs))
         (lenv (integer->serialized-form len))
         (outbuf (make-array (+ 1 (length lenv) len)
                             :element-type '(unsigned-byte 8))))
    (setf (aref outbuf 0) tag)
    (write-vector-on lenv outbuf :start 1)
    (write-vector-on outs outbuf :start (+ 1 (length lenv)))
    outbuf))

;;; ---------------------------------------------------------------------
;;; converting rows
;;; ---------------------------------------------------------------------

(defmethod row->serialized-form ((r row))
  (let* ((tag $tag-row)
         (fill 0)
         (elt-vals (seq:image #'val (elements r)))
         (elt-count (seq:length elt-vals))
         (elt-count-v (integer->serialized-form elt-count))
         (elt-vecs (as 'list (seq:image #'string->serialized-form elt-vals)))
         (outbuf (make-array (+ 1 (length elt-count-v) (sum-vector-lengths elt-vecs))
                             :element-type '(unsigned-byte 8))))
    (setf (aref outbuf fill) tag)
    (incf fill)
    (write-vector-on elt-count-v outbuf :start fill)
    (incf fill (length elt-count-v))
    (dolist (vec elt-vecs)
      (write-vector-on vec outbuf :start fill)
      (incf fill))
    outbuf))

;;; ---------------------------------------------------------------------
;;; converting columns
;;; ---------------------------------------------------------------------

(defmethod column->serialized-form ((col column))
  (let* ((tag $tag-column)
         (fill 0)
         (label (label col))
         (labelv (string->serialized-form label))
         (index (or (index col) 0))
         (indexv (integer->serialized-form index))
         (deleted? (deleted? col))
         (deletedv (bool->serialized-form deleted?))
         (outbuf (make-array (+ 1 (length labelv)(length indexv)(length deletedv))
                             :element-type '(unsigned-byte 8))))
    (setf (aref outbuf fill) tag)
    (incf fill)
    (write-vector-on labelv outbuf :start fill)
    (incf fill (length labelv))
    (write-vector-on indexv outbuf :start fill)
    (incf fill (length indexv))
    (write-vector-on deletedv outbuf :start fill)
    outbuf))

;;; ---------------------------------------------------------------------
;;; converting to and from serialized binary
;;; ---------------------------------------------------------------------

(defun columns->serialized-form (cols)
  (let* ((serialized-cols (as 'list (seq:image #'column->serialized-form cols)))
         (fill 0)
         (outbuf (make-array (sum-vector-lengths serialized-cols)
                             :element-type '(unsigned-byte 8))))
    (dolist (colv serialized-cols)
      (write-vector-on colv outbuf :start fill)
      (incf fill (length colv)))
    outbuf))

(defun rows->serialized-form (rows)
  (let* ((serialized-rows (as 'list (seq:image #'row->serialized-form rows)))
         (fill 0)
         (outbuf (make-array (sum-vector-lengths serialized-rows)
                             :element-type '(unsigned-byte 8))))
    (dolist (rowv serialized-rows)
      (write-vector-on rowv outbuf :start fill)
      (incf fill (length rowv)))
    outbuf))

(defmethod to-serialized-form ((m model))
  (let* ((fill 0)
         (format-sentinel $delectus-format-sentinel)
         (format-version (vector (current-store-format-version)))
         (columns (columns->serialized-form (as 'list (columns m))))
         (rows (rows->serialized-form (as 'list (rows m))))
         (outbuf (make-array (+ (length format-sentinel) (length format-version) (length columns) (length rows))
                             :element-type '(unsigned-byte 8))))
    (write-vector-on format-sentinel outbuf :start fill)
    (incf fill (length format-sentinel))
    (write-vector-on format-version outbuf :start fill)
    (incf fill (length format-version))
    (write-vector-on columns outbuf :start fill)
    (incf fill (length columns))
    (write-vector-on rows outbuf :start fill)
    outbuf))

(defmethod from-serialized-form ()
  )

;;; ---------------------------------------------------------------------
;;; storing and loading models
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

(defmethod load-data ((path pathname))
  (with-open-file (in path :direction :input
                      :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (read-sequence buffer in)
      buffer)))

(defmethod load-data ((path string))
  (reload m (pathname path)))

(defmethod load-model ((path pathname))
  (from-serialized-form (load-data path)))

(defmethod load-model ((path string))
  (load-data (pathname path)))

