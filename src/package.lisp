(in-package :cl-user)

(defpackage "DELECTUS"
  (:use :cl :capi :folio.as :folio.functions)
  (:import-from :folio.fn #:$ #:^)
  (:import-from :cl-user #:resource))

