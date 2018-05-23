(in-package :engine)

(defun str (&rest strings)
  (apply 'concatenate 'string
         (mapcar (lambda (s)(format nil "~A" s))
                 strings)))

(defun hash (bytes)
  (sha3:sha3-digest-vector bytes :output-bit-length 256))

(defun now-fractional-seconds ()
  (let* ((current-real-time (get-internal-real-time))
         (current-real-seconds (truncate current-real-time
                                         internal-time-units-per-second))
         (current-offset (- current-real-time (* current-real-seconds
                                                 internal-time-units-per-second))))
    (/ current-offset 1000.0)))

(defun timestamp-now ()
  (format nil "~A" (local-time:timestamp+ (local-time:now)
                                          (now-fractional-seconds)
                                          :sec)))

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
