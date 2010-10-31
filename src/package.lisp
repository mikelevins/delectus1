(in-package :cl-user)

(defpackage "DELECTUS"
  (:use :cl :capi :objc :folio.as)
  (:shadow "ELEMENT")
  (:import-from :folio.fn #:$ #:^)
  (:import-from :cl-user))

