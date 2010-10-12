(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; button utils
;;; ---------------------------------------------------------------------

(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageAbove 5)

(defun trash-button (pane view)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource "images/trashempty48.png"))))
  (objc:invoke view "setAlternateImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring "images/trashfull48.png")))
  view)

(defun top-button (label image altimage pane view)
  (setf view (objc:invoke view "init"))
  (objc:invoke view "setBordered:" nil)
  (objc:invoke view "setTitle:" label)
  (objc:invoke view "setImagePosition:" $NSImageAbove)
  (objc:invoke view "setButtonType:" $NSMomentaryChangeButton)
  (objc:invoke view "setImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource image))))
  (objc:invoke view "setAlternateImage:"
               (objc:invoke (objc:invoke "NSImage" "alloc")
                            "initByReferencingFile:" (namestring (resource altimage))))
  view)

;;; ---------------------------------------------------------------------
;;; view utils
;;; ---------------------------------------------------------------------

(define-objc-class data-source ()
  ((model :accessor model :initarg :model :initform nil))
  (:objc-class-name "DataSource"))

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self data-source)
     (table-view objc-object-pointer))
  5)

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self data-source)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (string-to-ns-string "Hello"))

(defparameter $foo nil)

(defun init-row-pane (pane view)
  (let* ((table-view (alloc-init-object "NSTableView"))
         (view (invoke view "init"))
         (source (retain (alloc-init-object "DataSource"))))
    (invoke table-view "setUsesAlternatingRowBackgroundColors:" t)
    (invoke view "setDocumentView:" table-view)
    (invoke table-view "setDataSource:" source)
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
                   :init-function (fun:partial 'top-button "Add Row" "images/add.png"  "images/addhl.png"))
   (delete-row-button cocoa-view-pane :view-class "NSButton"
                      :init-function (fun:partial 'top-button "Delete Row" "images/del.png"  "images/delhl.png"))
   (add-column-button cocoa-view-pane :view-class "NSButton" 
                      :init-function (fun:partial 'top-button "Add Col" "images/add.png"  "images/addhl.png"))
   (delete-column-button cocoa-view-pane :view-class "NSButton" 
                         :init-function (fun:partial 'top-button "Delete Col" "images/del.png" "images/delhl.png"))
   ;; main row
   (row-pane cocoa-view-pane :view-class "NSScrollView" :reader row-pane
             :init-function 'init-row-pane)
   ;; bottom row
   (trash-button cocoa-view-pane :view-class "NSButton"
                 :init-function 'trash-button)
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
  (:default-initargs :title "Delectus" :width 700 :height 400 :initial-focus 'filter-field
                     :window-styles '(:internal-borderless :textured-background)))

;;; (setq $w (contain (make-instance 'delectus-window)))
