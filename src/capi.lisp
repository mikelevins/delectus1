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

(define-interface delectus-ui ()
  ;; -- slots ---------------------------------------------
  ((document-path :accessor document-path :initform nil :initarg :document-path))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :header-args '(:selection-callback :sort)
                  :sort-descriptions (compute-column-sort-descriptions interface)
                  :columns (compute-column-descriptions interface)
                  :items (visible-delectus-rows (document-path interface))))

  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 800 :height 600))

(defmethod initialize-instance :after ((ui delectus-ui) &rest initargs &key &allow-other-keys)
  (assert (document-path ui)() "You must supply the pathname of a Delectus 2.x document to create a Delectus window")
  (setf (interface-title ui)
        (namestring (document-path ui))))

(defmethod compute-column-descriptions ((ui delectus-ui))
  (let* ((column-labels (visible-delectus-columns (document-path ui))))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))

(defun element-getter (n)
  #'(lambda (it)(elt it n)))

(defmethod compute-column-sort-descriptions ((ui delectus-ui))
  (let* ((column-labels (visible-delectus-columns (document-path ui))))
    (loop for i from 0 below (length column-labels)
          collect (capi:make-sorting-description :type (elt column-labels i)
                                                 :key (element-getter i)
                                                 :sort 'string-lessp
                                                 :reverse-sort 'string-greaterp))))

;;; (setf $doc (contain (make-instance 'delectus-ui)))
;;; (time (setf $doc (contain (make-instance 'delectus-ui :document-path "/Users/mikel/Desktop/junior-movies.delectus2"))))
;;; (visible-delectus-columns "/Users/mikel/Desktop/junior-movies.delectus2")

;;; (time (setf $doc (contain (make-instance 'delectus-ui :document-path "/Users/mikel/Desktop/zipcode_20k.delectus2"))))
