;;;; ***********************************************************************
;;;;
;;;; Name:          capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Lispworks CAPI UI
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; TODO: this interim implementation only works with the
;;;       junior's movie files; have to fogire out how
;;;       to construct sort descriptions for arbitrary
;;;       databases
(defparameter *sort-descriptions*
  (list
   (capi:make-sorting-description :type "Title"
                                  :key 'first
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Star"
                                  :key 'second
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Costar"
                                  :key 'third
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Subject"
                                  :key 'fourth
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)))

(define-interface delectus-ui ()
  ;; -- slots ---------------------------------------------
  ((document-path :accessor document-path :initform nil :initarg :document-path))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :header-args '(:selection-callback :sort)
                  :sort-descriptions *sort-descriptions*
                  :columns (compute-column-descriptions interface)
                  :items (visible-delectus-rows (document-path interface))))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

(defmethod initialize-instance :after ((ui delectus-ui) &rest initargs &key &allow-other-keys)
  (assert (document-path ui)() "You must supply the pathname of a Delectus 2.x document to create a Delectus window")
  (setf (interface-title ui)
        (namestring (document-path ui))))

(defmethod compute-column-descriptions ((ui delectus-ui))
  (let* ((column-labels (visible-delectus-columns (document-path ui))))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))

;;; (setf $doc (contain (make-instance 'delectus-ui)))
;;; (time (setf $doc (contain (make-instance 'delectus-ui :document-path "/Users/mikel/Desktop/junior-movies.delectus2"))))
;;; (visible-delectus-columns "/Users/mikel/Desktop/junior-movies.delectus2")
