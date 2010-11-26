;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       storing and reloading delectus data
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod store ((pres presentation)(out stream))
  (write-sequence (to-serialized-form pres) out))

(defmethod reload-presentation ((in stream))
  (let* ((inbuf (make-array (file-length in) :element-type (stream-element-type in)))
         (bytes (read-sequence inbuf in)))
    (from-serialized-form bytes)))
