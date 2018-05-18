;;; identity.lisp

;;; generate identities for Delectus' identifiable objects. An
;;; identity is randomly-generated in a manner similar to a v4 uuid,
;;; but all bits are random, unlike a v4 uuid, and the canonical
;;; storage format is a 64-character hex string (without hyphens).

(in-package :engine)

(defparameter *identity-random-state* nil)

(defun make-identity ()
  (unless *identity-random-state*
    (setf *identity-random-state*
          (make-random-state t)))
  (let* ((*print-base* 16)
         (bytes (vector (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*)
                        (random #xffff *identity-random-state*))))
    (string-downcase
     (with-output-to-string (out)
       (loop for b across bytes
          do (write b :stream out))))))
