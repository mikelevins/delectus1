;;; computing state tokens

(in-package :engine)

;;; HOWTO:
;;; conspack-encode the whole changelog, yielding a bytevector
;;; compute an sha3 digest of the bytevector, yielding a hashed bytevector
;;; write each byte in order to an output string in hexadecimal notation.
;;; the output string is the state token.
(defun compute-state-token (changelog)
  (let ((*print-base* 16)
        (bytes (hash (encode changelog))))
    (with-output-to-string (out)
      (loop for b across bytes
         do (write b :stream out)))))

;;; (compute-state-token nil)
;;; (compute-state-token `((message-add-row ,$list-id ,$state-token)))
