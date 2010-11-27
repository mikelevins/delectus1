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

(defparameter $delectus-format-sentinel "DLCT")
(defparameter $delectus-format-version 4)

(defmethod write-data (data (out stream))
  (s-serialization:serialize-sexp data out))

(defmethod store ((pres presentation)(path pathname))
  (with-open-file (out path :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-data $delectus-format-sentinel out)
    (write-data $delectus-format-version out)
    (write-data pres out)))

(defmethod store ((pres presentation)(path string))
  (store pres (pathname path)))

(defmethod convert-delectus-file-format ((in stream)(version integer))
  (error "Delectus format conversion not yet implemented"))

(defmethod reload-presentation ((in stream))
  (let ((format-sentinel (s-serialization:deserialize-sexp in)))
    (if (equalp format-sentinel $delectus-format-sentinel)
        (let ((format-version (s-serialization:deserialize-sexp in)))
          (if (eql format-version $delectus-format-version)
              (s-serialization:deserialize-sexp in)
              (convert-delectus-file-format in format-version)))
        (error "Invalid Delectus file format"))))

(defmethod reload-presentation ((path pathname))
  (with-open-file (in path :direction :input)
    (reload-presentation in)))

(defmethod reload-presentation ((path string))
  (reload-presentation (pathname path)))
