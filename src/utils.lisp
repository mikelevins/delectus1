;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       general-purpose utility functions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;;  file utilities
;;; ---------------------------------------------------------------------

;;;  file-pathname-p (pathname)
;;; ---------------------------------------------------------------------
;;; returns true if the pathname's name or type part is nonempty
;;; does not check whether the named file actually exists

(defun file-pathname-p (pathname)
  (when pathname
    (let* ((pathname (pathname pathname))
           (name (pathname-name pathname))
           (type (pathname-type pathname)))
      (when (or (not (member name '(nil :unspecific "") :test 'equal))
                (not (member type '(nil :unspecific "") :test 'equal)))
        pathname))))

;;; ---------------------------------------------------------------------
;;;  string utilities
;;; ---------------------------------------------------------------------

(defun join-strings (cupola strings)
  (cond ((null strings) nil)
        ((null (rest strings)) (first strings))
        (t (reduce (lambda (left right)(concatenate 'string left cupola right))
                   strings))))

;;; (join-strings ", " (list "apple" "banana" "cherry"))
;;; (join-strings "" (list "apple" "banana" "cherry"))

(defun str (&rest vals)
  (join-strings ""
                (mapcar (lambda (v)(format nil "~A" v))
                        vals)))

;;; (str 1 2 3)
;;; (str "1" "2" "3")
;;; (str '("CREATE TABLE " "`list_data` " "(`optype` TEXT, `opid` TEXT, `origin` TEXT, `revision` INTEGER, `timestamp` TEXT, `item` TEXT, `name` TEXT, `deleted` TEXT, `peer` TEXT);"))

;;; ---------------------------------------------------------------------
;;;  time utilities
;;; ---------------------------------------------------------------------


;;; now-timestamp
;;; ---------------------------------------------------------------------

(defun now-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))

;;; (now-timestamp)
