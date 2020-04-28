;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       delectus 2
;;;; Purpose:       delectus package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage #:delectus
  (:use #:cl #:sqlite #:cl-emb #:named-readtables)
  (:import-from #:fset
                #:compare #:convert #:do-map #:domain
                #:lookup #:wb-map #:with)
  (:export #:bind
           #:count-latest-filtered-items
           #:from-json
           #:db-count-latest-filtered-items
           #:db-get-latest-columns
           #:db-get-latest-items-userdata
           #:db-get-latest-listname
           #:db-get-latest-userdata-columns-data
           #:db-get-userdata-column-widths
           #:get-key
           #:get-latest-columns
           #:get-latest-items-userdata
           #:get-latest-listname
           #:get-latest-userdata-columns-data
           #:get-userdata-column-widths
           #:join-strings
           #:op-name
           #:op-userdata
           #:with-open-database))

(defpackage #:sqlgen
  (:use #:cl #:sqlite #:named-readtables))

(defpackage #:delectus-ui
  (:nicknames #:ui)
  (:use #:cl #:sqlite #:cl-emb #:named-readtables #:delectus #:capi))
