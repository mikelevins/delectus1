;;;; ***********************************************************************
;;;;
;;;; Name:          system-node.lisp
;;;; Project:       delectus 2
;;;; Purpose:       compute and store an identifier for Delectus
;;;;                running on a given device for a given user
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; TODO: rewrite this to use proper platform-specific mechanisms instead of hardcoding it
(defun delectus-node-identity-path ()
  (pathname "/Users/mikel/Library/Application Support/Delectus/nodeid"))

(defun create-delectus-node-identity-file ()
  (with-open-file (out (delectus-node-identity-path) :direction :output)
    (format out "~A" (make-identity-string))))

(let ((nodeid nil))
  (defun delectus-node-identity ()
    (or nodeid
        (let ((idpath (probe-file (delectus-node-identity-path))))
          (unless idpath
            (create-delectus-node-identity-file))
          (with-open-file (in idpath)
            (setf nodeid
                  (string->identity (symbol-name (read in)))))
          nodeid))))
