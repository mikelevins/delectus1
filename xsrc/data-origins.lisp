;;;; ***********************************************************************
;;;;
;;;; Name:          data-origins.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for computing unique IDs for list files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; An origin is an identity computed as a hash from the Delectus node
;;; and the pathname of the list file. If the origin of two ops
;;; is different, then they were inserted in different files, and their
;;; revision numbers are independent.

