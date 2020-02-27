(ns delectus.errors)

;;; ---------------------------------------------------------------------
;;; general errors
;;; ---------------------------------------------------------------------

(defn error [message context]
  (throw (ex-info message (merge context {:error-signaled-by 'error}))))

(defn error-not-implemented [name]
  (throw (ex-info (str name " is not yet implemented. "))))

(defn error-if-empty [value message context]
  (if (empty? value)
    (throw (ex-info message (merge context {:error-signaled-by 'error-if-empty})))))

(defn error-if-nil [value message context]
  (if (nil? value)
    (throw (ex-info message (merge context {:error-signaled-by 'error-if-nil})))))

(defn error-if [test-val message context]
  (if test-val
    (throw (ex-info message (merge context {:error-signaled-by 'error-if})))))

(defn error-if-not [test-val message context]
  (if-not test-val
    (throw (ex-info message (merge context {:error-signaled-by 'error-if-not})))))



