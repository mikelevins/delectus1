;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          render-item.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       CAPI code for drawing Delectus items in output-panes
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod string-width ((pane capi:output-pane)(s string) &key font)
  (let ((font (or font (capi:simple-pane-font pane))))
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent pane s font)
      (- right left))))

(defmethod string-height ((pane capi:output-pane)(s string) &key font)
  (let ((font (or font (capi:simple-pane-font pane))))
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent pane s font)
      (- bottom top))))

(defmethod parse-frame-parameters ((params (eql t)))
  (values 1 1 1 1 :white :white :black :black))

(defmethod parse-frame-parameters ((params list))
  (values (getf params :left-thickness 1)
          (getf params :top-thickness 1)
          (getf params :right-thickness 1)
          (getf params :bottom-thickness 1)
          (getf params :left-color :white)
          (getf params :top-color :white)
          (getf params :right-color :black)
          (getf params :bottom-color :black)))

(defmethod render-item ((pane capi:output-pane)(item string) x y width height
                        &key
                        (background nil)(foreground :black)
                        (font nil)(frame nil)
                        (margin-left 0)(margin-top 0)
                        (margin-right 0)(margin-bottom 0))
  (let* ((font (or (and font
                       (gp:find-best-font pane font)) 
                  (capi:simple-pane-font pane)))
         (width (+ margin-left (string-width pane item :font font) margin-right))
         (height (+ margin-top (string-height pane item :font font) margin-bottom)))
    (when background
      (gp:with-graphics-state (pane :foreground background)
        (gp:draw-rectangle pane x y width height :filled t)))
    (gp:draw-string pane item (+ x margin-left)(- (+ y height) margin-bottom)
                    :font font)
    (when frame
      (multiple-value-bind (left-thickness top-thickness right-thickness bottom-thickness
                                           left-color top-color right-color bottom-color)
          (parse-frame-parameters frame)
        (let* ((y (- (+ y height) bottom-thickness))
               (height bottom-thickness))
          (gp:with-graphics-state (pane :foreground bottom-color :thickness bottom-thickness)
            (gp:draw-rectangle pane x y width height :filled t)))
        (let* ((x (- (+ x width) right-thickness))
               (width right-thickness))
          (gp:with-graphics-state (pane :foreground right-color :thickness right-thickness)
            (gp:draw-rectangle pane x y width height :filled t)))
        (let* ((width left-thickness))
          (gp:with-graphics-state (pane :foreground left-color :thickness left-thickness)
            (gp:draw-rectangle pane x y width height :filled t)))
        (let* ((height top-thickness))
          (gp:with-graphics-state (pane :foreground top-color :thickness top-thickness)
            (gp:draw-rectangle pane x y width height :filled t)))))))

#|

(let ((out (make-instance 'capi:output-pane 
                          :font (gp:make-font-description :family "Arial" :size 14 :weight :bold))))
  (setf (capi:output-pane-display-callback out)
        (lambda (out x y width height)
          (render-item out "Foo!" x y width height
                       :margin-left 2 :margin-top 2
                       :margin-right 2 :margin-bottom 6
                       :background :lightgray :frame '(:left-color :red :left-thickness 4)
                       :font (gp:make-font-description :family "Helvetica" :size 16 :weight :bold))))
  (capi:contain out))

|#