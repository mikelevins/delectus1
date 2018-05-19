;;; state.lisp
;;; computing state tokens

(in-package :engine)

;;; HOWTO:
;;; conspack-encode the whole changelog, yielding a bytevector
;;; compute an sha3 digest of the bytevector, yielding a hashed bytevector
;;; write each byte in order to an output string in hexadecimal notation.
;;; the output string is the state token.

(defun compute-state-token (changelog)
  (if (null changelog)
      nil
      (let ((*print-base* 16)
            (bytes (hash (encode changelog))))
        (with-output-to-string (out)
          (loop for b across bytes
             do (write b :stream out))))))

;;; (compute-state-token nil)
;;; (compute-state-token `((message-add-row ,$list-id ,$state-token)))

;;; A statelog is simply a list of recorded state tokens associated
;;; with the identifiable object for which the tokens have been
;;; computed. They are recorded in reverse order, by pushing new ones
;;; onto the front of the list.
