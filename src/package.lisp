;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage #:delectus
  (:use #:cl #:sqlite #:fare-csv #+capi #:capi))

(defpackage #:delectus.version
  (:use #:cl)
  (:nicknames #:version)
  (:export
   #:+delectus-major-version+
   #:+delectus-minor-version+
   #:+delectus-patch-version+
   #:+delectus-build-number+
   #:delectus-version
   #:delectus-version-string
   #:+delectus-format-version+ ; version number of the Delectus file format
   #:delectus-format-version-string ; version string for the delectus file format
   ))
