;;;; ***********************************************************************
;;;;
;;;; Name:          store-column-info.lisp
;;;; Project:       delectus 2
;;;; Purpose:       getting column-info from SQLite files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)


;;; SQLite column-info
;;; ---------------------------------------------------------------------

(defstruct (column-info (:constructor column-info (cid name type notnull dflt_value pk)))
  cid name type notnull dflt_value pk)

