;;; collections.lisp
;;; constructing, accessing, reading, and writing Delectus collections
;;; in Lisp

(in-package :delectus)

(defclass delectus-collection ()
  ((title :accessor title :initform "" :initarg :title)
   ))
