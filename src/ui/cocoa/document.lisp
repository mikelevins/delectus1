;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          views.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       cocoa-specific document code
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;;  document
;;; ---------------------------------------------------------------------

(define-objc-protocol "NSTableViewDataSource"
    :instance-methods (("numberOfRowsInTableView:" (:unsigned :long)
                                                   objc-object-pointer)
                       ("tableView:objectValueForTableColumn:row:" objc-object-pointer
                                                                   objc-object-pointer
                                                                   (:unsigned :long))
                       ("tableView:setObjectValue:forTableColumn:row:" :void
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       (:unsigned :long))))

(define-objc-class document ()
  ((name :reader name :initarg :name :initform nil)
   (changed? :accessor changed? :initform t)
   (pathname :accessor pathname :initarg :pathname :initform nil)
   (window :reader window :initarg :window :initform nil)
   (presentation :reader presentation :initarg :presentation :initform (make-instance 'presentation)))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(defparameter $NSChangeDone 0)

(defmethod initialize-instance :after ((doc document) &rest initargs &key (name "Untitled") (presentation nil)
                                       &allow-other-keys)
  (let ((pres (or presentation (make-instance 'presentation))))
    (setf (slot-value doc 'name)  name)
    (setf (slot-value doc 'window)(make-instance 'delectus-window :document doc))))

(defmethod notify-redisplay-document ((doc document))
  (invoke (invoke (cocoa-view-pane-view (row-pane (window doc))) "documentView")
          "reloadData"))

;;; ---------------------------------------------------------------------
;;;  NSDataSource methods

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self document)
     (table-view objc-object-pointer))
  (count-rows self))

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self document)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (let* ((col-name (invoke-into 'string (invoke column "headerCell") "stringValue"))
         (val (value-at self col-name row)))
    (if (stringp val)
        val
        (princ-to-string val))))

(define-objc-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((self document)
     (table-view objc-object-pointer)
     (object-value objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  (let* ((col-name (invoke-into 'string (invoke column "headerCell") "stringValue"))
         (model-value (invoke-into 'string (invoke "NSString" "alloc") "initWithString:" object-value)))
    (put-value-at! self col-name row model-value)))

;;; ---------------------------------------------------------------------
;;;  target/action methods

(define-objc-method ("addRow:" :void)
    ((self document)
     (sender objc-object-pointer))
  (document-add-row! self))

(define-objc-method ("deleteRow:" :void)
    ((self document)
     (sender objc-object-pointer))
  (delete-selected-row! self))

(define-objc-method ("addColumn:" :void)
    ((self document)
     (sender objc-object-pointer))
  (document-add-column! self))

(define-objc-method ("deleteColumn:" :void)
    ((self document)
     (sender objc-object-pointer))
  (delete-selected-column! self))

(define-objc-method ("toggleTrash:" :void)
    ((self document)
     (sender objc-object-pointer))
  (toggle-trash self))
