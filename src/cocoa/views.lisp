(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; delectus views
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; button setup
;;; ---------------------------------------------------------------------

;;; button parameters
(defparameter $NSToggleButton 2)
(defparameter $NSMomentaryChangeButton 5)
(defparameter $NSImageOnly 1)
(defparameter $NSImageAbove 5)

(defun button-init (&key (label "") target bordered(button-type $NSMomentaryChangeButton)
                    image altimage (image-position $NSImageAbove))
  (lambda (capi-pane objc-view)
    (setf objc-view (invoke objc-view "init"))
    (setf $last-target target)
    (invoke objc-view "setTarget:" (objc-object-pointer target))
    (invoke objc-view "setTitle:" label)
    (invoke objc-view "setImage:" image)
    (invoke objc-view "setAlternateImage:" altimage)
    (invoke objc-view "setButtonType:" button-type)
    (invoke objc-view "setBordered:" bordered)
    (invoke objc-view "setImagePosition:" image-position)
    objc-view))

;;; ---------------------------------------------------------------------
;;; NSTableView setup
;;; ---------------------------------------------------------------------

(defun table-init (&key data-source)
  (lambda (capi-pane objc-view)
    (let* ((table-view (alloc-init-object "NSTableView"))
           (scrollview (invoke objc-view "init")))
      (invoke scrollview "setHasVerticalScroller:" t)
      (invoke scrollview "setHasHorizontalScroller:" t)
      (invoke table-view "setUsesAlternatingRowBackgroundColors:" t)
      (invoke scrollview "setDocumentView:" table-view)
      (invoke table-view "setDataSource:" (objc-object-pointer data-source))
      scrollview)))

(defun setup-columns (row-pane doc)
  (let* ((table-view (invoke (cocoa-view-pane-view row-pane) "documentView"))
         (pres (presentation doc))
         (cols (presented-columns pres)))
    (seq:image (^ (col)
                 (let* ((colname (label col))
                        (table-column (invoke (invoke "NSTableColumn" "alloc")
                                              "initWithIdentifier:" colname))
                        (header-cell (invoke table-column "headerCell")))
                   (invoke header-cell "setStringValue:" colname)
                   (invoke table-view "addTableColumn:" table-column)))
               cols)))