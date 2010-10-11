(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; global parameters
;;; ---------------------------------------------------------------------

(defparameter $bggray (color:make-rgb #xe8 #xe8 #xe8))

;;; ---------------------------------------------------------------------
;;; button utils
;;; ---------------------------------------------------------------------

(defparameter $NSMomentaryChangeButton 5)

(defun %button (pane view image &optional altimage)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring image)))
  (when altimage 
    (objc:invoke view "setAlternateImage:"
                 (objc:invoke (objc:invoke "NSImage" "alloc")
                              "initByReferencingFile:" (namestring altimage))))
  view)

(defun %mcbutton (pane view label image &optional altimage)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setTitle:" label)
  (objc:invoke view "setButtonType:" $NSMomentaryChangeButton)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring image)))
  (when altimage 
    (objc:invoke view "setAlternateImage:"
                 (objc:invoke (objc:invoke "NSImage" "alloc")
                              "initByReferencingFile:" (namestring altimage))))
  view)

;;; ---------------------------------------------------------------------
;;; main UI
;;; ---------------------------------------------------------------------

(define-interface delectus-window ()
  ;; slots
  ((model :reader model :initarg :model :initform nil))
  ;; panes
  (:panes
   ;; top row
   (add-row-button cocoa-view-pane :view-class "NSButton" 
                   :init-function (^ (pane view)(%mcbutton pane view "Add Row"
                                                           (resource "images/add.png")
                                                           (resource "images/addhl.png"))))
   (delete-row-button cocoa-view-pane :view-class "NSButton"
                      :init-function (^ (pane view)(%mcbutton pane view "Delete Row"
                                                              (resource "images/del.png")
                                                              (resource "images/delhl.png"))))
   (add-column-button cocoa-view-pane :view-class "NSButton" 
                      :init-function (^ (pane view)(%mcbutton pane view "Add Column"
                                                              (resource "images/add.png")
                                                              (resource "images/addhl.png"))))
   (delete-column-button cocoa-view-pane :view-class "NSButton" 
                         :init-function (^ (pane view)(%mcbutton pane view "Delete Column"
                                                                 (resource "images/del.png")
                                                                 (resource "images/delhl.png"))))
   ;; main row
   (row-table cocoa-view-pane :view-class "NSTableView")
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton"
                 :init-function (^ (pane view)
                                  (%button pane view
                                           (resource "images/trashempty48.png")
                                           (resource "images/trashfull48.png"))))
   (filter-field cocoa-view-pane :view-class "NSSearchField"))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster)
            :external-min-height 64 :external-max-height 64)
   (table-row row-layout '(row-table))
   (bottom-row row-layout '(trash-cluster nil filter-field)
            :external-min-height 64 :external-max-height 64)
   ;; control clusters
   (row-cluster row-layout '(add-row-button delete-row-button) :adjust :center
                :external-min-width 128 :external-max-width 128)
   (column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
                   :external-min-width 128 :external-max-width 128)
   (trash-cluster row-layout '(trash-button) :adjust :center
                  :external-min-width 128 :external-max-width 128))
  ;; defaults
  (:default-initargs :width 700 :height 400))

;;; (setq $w (contain (make-instance 'delectus-window)))
