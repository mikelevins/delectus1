(in-package :engine)

(defun str (&rest strings)
  (apply 'concatenate 'string
         (mapcar (lambda (s)(format nil "~A" s))
                 strings)))

(defun hash (bytes)
  (sha3:sha3-digest-vector bytes :output-bit-length 256))
