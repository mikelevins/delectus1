(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; global parameters
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; button utils
;;; ---------------------------------------------------------------------

(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageAbove 5)

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
  (objc:invoke view "setImagePosition:" $NSImageAbove)
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
;;; view utils
;;; ---------------------------------------------------------------------

(defun init-row-pane (pane view)
  (let* ((table-view (objc:alloc-init-object "NSTableView"))
         (view (objc:invoke view "init")))
    (objc:invoke table-view "setUsesAlternatingRowBackgroundColors:" t)
    (objc:invoke view "setDocumentView:" table-view)
    view))

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
   (row-pane cocoa-view-pane :view-class "NSScrollView" :reader row-pane
             :init-function 'init-row-pane)
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
            :external-min-height 60 :external-max-height 60)
   (table-row row-layout '(row-pane))
   (bottom-row row-layout '(trash-cluster nil filter-field)
            :external-min-height 56 :external-max-height 56)
   ;; control clusters
   (row-cluster row-layout '(add-row-button delete-row-button) :adjust :center
                :external-min-width 196 :external-max-width 196)
   (column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
                   :external-min-width 196 :external-max-width 196)
   (trash-cluster row-layout '(trash-button) :adjust :center
                  :external-min-width 84 :external-max-width 84))
  ;; defaults
  (:default-initargs :title "Delectus" :width 700 :height 400 :initial-focus 'filter-field))

;;; (setq $w (contain (make-instance 'delectus-window)))
