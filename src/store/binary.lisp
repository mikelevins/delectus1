;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          binary.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       binary I/O
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; utils
;;; =====================================================================

(defmethod capacity ((vec vector))
  (array-dimension vec 0))

;;; =====================================================================
;;; byte I/O
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; writing bytes
;;; ---------------------------------------------------------------------

(defmethod write-one-byte ((byte integer)(out vector))
  (assert (<= 0 byte 255)() "Byte value ~S is out of range" byte)
  (vector-push-extend byte out)
  out)

(defmethod write-bytes ((bytes vector)(out vector))
  (assert (fill-pointer out)() "WRITE-BYTES can write to a vector only if it has a fill-pointer")
  (assert (adjustable-array-p out)() "WRITE-BYTES can write to a vector only if it is adjustable")
  (let* ((len (length bytes))
         (fp (fill-pointer out))
         (room (- (capacity out) fp)))
    (when (< room len)
      (adjust-array out (list (* 2 (capacity out)))))
    (let ((room (- (capacity out) fp)))
      (when (< room len)
        (error "WRITE-BYTES is unable to make enough room in an output vector")))
    (replace out bytes :start1 fp)
    (incf (fill-pointer out) len)
    out))

(defmethod write-bytes ((bytes vector)(out stream))
  (assert (output-stream-p out)() "Can't write bytes to ~S; it is not an output stream" out)
  (write-sequence bytes out))

(defmethod write-one-byte ((byte integer)(out stream))
  (assert (<= 0 byte 255)() "Byte value ~S is out of range" byte)
  (write-byte byte out)
  out)

;;; ---------------------------------------------------------------------
;;; reading bytes
;;; ---------------------------------------------------------------------

(defmethod read-bytes ((in stream))
  (let ((inbuf (make-array (file-length in) :element-type '(unsigned-byte 8))))
    (read-sequence inbuf in)
    inbuf))

(defmethod read-one-byte ((in stream))
  (read-byte in))

;;; =====================================================================
;;; serialization buffers
;;; =====================================================================

(defun make-serialization-buffer (len)
  (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(defun clear-serialization-buffer (buf)
  (setf (fill-pointer buf) 0)
  buf)

;;; =====================================================================
;;; type tags
;;; =====================================================================

(defmethod write-tag ((tag integer)(out vector))
  (write-one-byte tag out))

(defmethod read-tag ((in stream))
  (read-one-byte out))

;;; =====================================================================
;;; structured types
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; serialization tags
;;; ---------------------------------------------------------------------

(defparameter $tag-bool 0)
(defparameter $tag-uint32 1)
(defparameter $tag-string 2)
(defparameter $tag-vector 3)

;;; ---------------------------------------------------------------------
;;; writing structured data
;;; ---------------------------------------------------------------------

(defmethod write-binary ((b (eql t)) (out vector))
  (write-tag $tag-bool out)
  (write-one-byte 1 out)
  out)

(defmethod write-binary ((b (eql nil)) (out vector))
  (write-tag $tag-bool out)
  (write-one-byte 0 out)
  out)

(defmethod write-binary ((i integer)(out vector))
  (assert (<= 0 i #xffffffff)() "integer ~A is out of range" i)
  (write-tag $tag-uint32 out)
  (write-one-byte (ldb (byte 8 24) i) out)
  (write-one-byte (ldb (byte 8 16) i) out)
  (write-one-byte (ldb (byte 8 8) i) out)
  (write-one-byte (ldb (byte 8 0) i) out)
  out)

(defmethod write-binary ((s string) (out vector))
  (let ((len (length s)))
    (write-tag $tag-string out)
    (write-binary len out)
    (loop for ch across s
       do (write-one-byte (char-code ch) out))
    out))

(defmethod write-binary ((v vector) (out vector))
  (let ((len (length v)))
    (write-tag $tag-vector out)
    (write-binary len out)
    (loop for e across v
       do (write-binary e out))
    out))

;;; ---------------------------------------------------------------------
;;; reading structured data
;;; ---------------------------------------------------------------------
"
(defmethod read-boolean-from-buffer (inbuf pos)
  (values (ecase (aref inbuf (1+ pos))
            ((0) nil)
            ((1) t))
          (+ pos 2)))"

(defun read-uint32-from-buffer (inbuf pos)
  (let ((count 0))
    (setf (ldb (byte 8 24) count)(aref inbuf (+ pos 1)))
    (setf (ldb (byte 8 16) count)(aref inbuf (+ pos 2)))
    (setf (ldb (byte 8 8) count)(aref inbuf (+ pos 3)))
    (setf (ldb (byte 8 0) count)(aref inbuf (+ pos 4)))
    (values count (+ pos 5))))

(defun read-string-from-buffer (inbuf pos)
  (multiple-value-bind (strlen new-pos)
      (read-uint32-from-buffer inbuf (1+ pos))
    (let* ((str-bytes (subseq inbuf new-pos (+ new-pos strlen)))
           (str (make-string strlen)))
      (loop for i from 0 upto (1- strlen)
         do (setf (aref str i)(code-char (aref str-bytes i))))
      (values str (+ new-pos strlen)))))

(defun read-vector-from-buffer (inbuf pos)
  (multiple-value-bind (vlen new-pos)
      (read-uint32-from-buffer inbuf (1+ pos))
    (let ((elts nil)
          (pos new-pos))
      (loop
         for i from vlen downto 1
         do
           (multiple-value-bind (val new-pos)
               (read-tagged-data inbuf (elt inbuf pos) pos)
             (setf pos new-pos)
             (setf elts (cons val elts))))
      (values (make-array (length elts) :initial-contents (reverse elts))
              pos))))

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-bool))(index integer))
  (read-boolean-from-buffer inbuf index))

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-uint32))(index integer))
  (read-uint32-from-buffer inbuf index))

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-string))(index integer))
  (read-string-from-buffer inbuf index))

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-vector))(index integer))
  (read-vector-from-buffer inbuf index))

(defmethod read-binary-data ((inbuf array))
  (let ((pos 0))
    (let ((tag (elt inbuf pos)))
           (read-tagged-data inbuf tag pos))))

(defmethod read-binary ((in stream))
  (let ((inbuf (make-array (file-length in) :element-type '(unsigned-byte 8))))
    (read-sequence inbuf in)
    (read-binary-data inbuf)))

#| testing

(setq $buf (make-serialization-buffer 1024))

(clear-serialization-buffer $buf)
(write-binary t $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary nil $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary 25 $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary 1025 $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary 123456789 $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary "Foo, bar, and baz" $buf)
(read-binary-data $buf)

(clear-serialization-buffer $buf)
(write-binary #(0 1 0 2 0 3) $buf)
(read-binary-data $buf)

|#