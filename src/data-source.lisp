(in-package :delectus)

(define-objc-protocol "NSTableViewDataSource"
    :instance-methods (("numberOfRowsInTableView:" (:unsigned :long)
                                                   objc-object-pointer)
                       ("tableView:objectValueForTableColumn:row:" objc-object-pointer
                                                                   objc-object-pointer
                                                                   objc-object-pointer
                                                                   (:unsigned :long))
                       ("tableView:setObjectValue:forTableColumn:row:" :void
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       objc-object-pointer
                                                                       (:unsigned :long))))

(define-objc-class data-source ()
  ((model :accessor model :initarg :model :initform nil))
  (:objc-class-name "DataSource")
  (:objc-protocols "NSTableViewDataSource"))

(define-objc-method ("numberOfRowsInTableView:" (:unsigned :long))
    ((self data-source)
     (table-view objc-object-pointer))
  (seq:length (presentation (model self))))

(define-objc-method ("tableView:objectValueForTableColumn:row:" objc-object-pointer)
    ((self data-source)
     (table-view objc-object-pointer)
     (column objc-object-pointer)
     (row (:unsigned :long)))
  "Hello from Lisp!")

