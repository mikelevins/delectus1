;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-pane.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       a display pane for delectus data
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defun string-width (pane string)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent pane string (capi:simple-pane-font pane))
    (- right left)))

(defun string-height (pane string)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent pane string (capi:simple-pane-font pane))
    (- bottom top)))

(defclass delectus-pane (capi:output-pane)
  ((delectus :accessor delectus :initform nil :initarg :delectus)
   (frame :accessor frame :initform nil :initarg :frame)
   (frame-color :accessor frame-color :initform :black :initarg :frame-color)
   (item-frame :accessor item-frame :initform nil :initarg :item-frame)
   (item-frame-color :accessor item-frame-color :initform :black :initarg :item-frame-color)
   (item-background :accessor item-background :initform :white :initarg :item-background)
   (item-left-margin :accessor item-left-margin :initform 4)
   (item-top-margin :accessor item-top-margin :initform 4)
   (item-right-margin :accessor item-right-margin :initform 4)
   (item-bottom-margin :accessor item-bottom-margin :initform 4)
   (item-stops :accessor item-stops :initform nil :initarg :item-stops))
  (:default-initargs :font (gp:make-font-description :family "Arial" :size 14)
     :display-callback #'display-delectus))

(defmethod draw-delectus-item ((pane delectus-pane)(item item) bounds)
  (destructuring-bind (left top width height) bounds
    (when (item-background pane)
      (gp:with-graphics-state (pane :foreground (item-background pane))
        (gp:draw-rectangle pane left top width height :filled t)))
    (gp:draw-string pane (value item)
                    (+ left (item-left-margin pane))
                    (- (+ top height)
                       (item-bottom-margin pane)))
    (when (item-frame pane)
      (let* ((frame-val (item-frame pane))
             (frame-width (if (numberp frame-val) frame-val 1))
             (frame-color (or (item-frame-color pane) :white)))
        (gp:with-graphics-state (pane :foreground frame-color)
          (gp:draw-rectangle pane left top width height 
                             :filled nil :thickness frame-width))))))

(defmethod display-delectus ((pane delectus-pane) x y width height)
  (loop
     for item in (items (delectus pane))
     and item-bounds in (compute-item-bounds pane)
     do (draw-delectus-item pane item item-bounds))
  (when (frame pane)
    (let* ((frame-val (frame pane))
           (frame-width (if (numberp frame-val) frame-val 1))
           (frame-color (frame-color pane)))
      (capi:with-geometry pane
        (gp:with-graphics-state (pane :foreground frame-color)
          (gp:draw-rectangle pane 0 0 
                            (1- capi:%width%)
                            (1- capi:%height%)
                            :filled nil :thickness frame-width))))))

(defclass horizontal-delectus-pane (delectus-pane)())
(defclass vertical-delectus-pane (delectus-pane)())

(defmethod compute-item-bounds ((pane horizontal-delectus-pane))
  (let ((items (items (delectus pane))))
    (when items
      (let* ((item-count (length items))
             (item-stops (item-stops pane))
             (item-stops-count (length item-stops))
             (item-lefts (make-array item-count :initial-element 0))
             (item-widths (loop for it in items
                             collect (+ (item-left-margin pane)
                                        (string-width pane (value it))
                                        (item-right-margin pane))))
             (item-tops (seq:repeat item-count 0))
             (item-heights (mapcar (^ (it)
                                     (+ (item-top-margin pane)
                                        (string-height pane (value it))
                                        (item-bottom-margin pane))) items)))
        (when (> item-count 1)
          (loop 
             for i from 0 upto (- item-count 2)
             and width in item-widths
             do (progn
                  (when (< i item-stops-count)
                    (setf (aref item-lefts i)(elt item-stops i)))
                  (setf (aref item-lefts (1+ i))
                        (+ (aref item-lefts i)
                           (elt item-widths i))))))
        (loop
           for left across item-lefts
           and top across item-tops
           and width in item-widths
           and height in item-heights
           collect (list left top width height))))))

(defmethod compute-item-bounds ((pane vertical-delectus-pane))
  (let ((items (items (delectus pane))))
    (when items
      (let* ((item-count (length items))
             (item-stops (item-stops pane))
             (item-stops-count (length item-stops))
             (item-lefts (seq:repeat item-count 0))
             (item-widths (loop for it in items
                             collect (+ (item-left-margin pane)
                                        (string-width pane (value it))
                                        (item-right-margin pane))))
             (item-tops (make-array item-count :initial-element 0))
             (item-heights (mapcar (^ (it)
                                     (+ (item-top-margin pane)
                                        (string-height pane (value it))
                                        (item-bottom-margin pane))) items)))
        (when (> item-count 1)
          (loop 
             for i from 0 upto (- item-count 2)
             and height in item-heights
             do (progn
                  (when (< i item-stops-count)
                    (setf (aref item-tops i)(elt item-stops i)))
                  (setf (aref item-tops (1+ i))
                        (+ (aref item-tops i)
                           (elt item-heights i))))))
        (loop
           for left across item-lefts
           and top across item-tops
           and width in item-widths
           and height in item-heights
           collect (list left top width height))))))

#|
(setq $del (make-delectus "Fred" "Barney" "Wilma" "Betty" "Pebbles" "Bam Bam"))
(setq $w (capi:contain (make-instance 'horizontal-delectus-pane :delectus $del)))
(setq $w (capi:contain (make-instance 'vertical-delectus-pane :delectus $del)))

(setq $w (capi:contain (make-instance 'horizontal-delectus-pane :delectus $del
                                      :background :lightgray
                                      :item-frame 2 :item-frame-color :red)))


|#