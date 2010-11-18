;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          top-button.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       toolbar buttons
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod update-image ((btn top-button) img) nil)

(defmethod update-image ((btn top-button) (img gp:image))
  (setf (image btn) img)
  img)

(defmethod update-image ((btn top-button) (img pathname))
  (update-image btn (gp:load-image btn img)))

(defmethod update-image ((btn top-button) (img string))
  (update-image btn (pathname img)))

(defun display-top-button (btn x y width height)
  (let ((img (update-image btn (image btn))))
    (when img
      (gp:draw-image btn img
                     (- (/ width 2)
                        (/ (gp:image-width img) 2))
                     (- (/ height 2)
                        (/ (gp:image-height img) 2))))))

(defclass top-button (output-pane)
  ((image :accessor image :initarg :image :initform nil)
   (altimage :accessor altimage :initarg :altimage :initform nil))
  (:default-initargs :background :transparent :title-position :bottom :title-adjust :center
                     :display-callback #'display-top-button))

;;; (contain (make-instance 'top-button))
;;; (contain (make-instance 'top-button :image (resource "trashempty48.png")))

