(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; list model
;;; ---------------------------------------------------------------------

(defclass delectus-model ()
  (;; a file, stream, or URL from which to read data
   (source :accessor source :initarg :source :initform nil)
   ;; the last-read data
   (data :accessor data :initarg :data :initform nil)
   ;; when the data field was last updated
   (data-current :accessor data-current :initarg :data-current :initform nil)
   ;; a user-specified function that determines which data to present
   (filter-fn :accessor filter-fn :initarg :filter-fn :initform nil)
   ;; a user-specified function that determines how to order the data
   (sort-fn :accessor sort-fn :initarg :sort-fn :initform nil)
   ;; the data that are to be presented in the UI
   (presentation :accessor presentation :initform nil)
   ;; when the presentation field was last updated
   (presentation-current :accessor presentation-current :initarg :presentation-current :initform nil)))

