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
        (write-vector-on inv outv))
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

(defmethod serialize-boolean-to-vector ((b (eql t)) out)
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
  (dolist (col (as 'list cols))
    (serialize-column-to-buffer col out))
  out)

(defun serialize-row-to-buffer (row out)
  (let* ((tag $tag-row))
    (write-byte-on-vector tag out)
    (serialize-count-to-buffer (seq:length (elements row)) out)
    (dolist (el (as 'list (elements row)))
      (serialize-string-to-buffer (val el) out)))
  out)

(defun serialize-rows-to-buffer (rows out)
  (dolist (row (as 'list rows))
    (serialize-row-to-buffer row out))
  out)

;;; ---------------------------------------------------------------------
;;; to-serialized-form
;;; ---------------------------------------------------------------------

(defmethod guess-serialized-length ((m model))
  (+ (* 64 (seq:length (columns m)))
     (* 64 (seq:length (rows m)))))

(defmethod to-serialized-form ((m model))
  (let ((outbuf (make-serialization-buffer (guess-serialized-length m))))
    (serialize-delectus-format-to-buffer outbuf)
    (serialize-delectus-format-version-to-buffer outbuf)
    (serialize-columns-to-buffer (columns m) outbuf)
    (serialize-rows-to-buffer (rows m) outbuf)
    outbuf))

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

