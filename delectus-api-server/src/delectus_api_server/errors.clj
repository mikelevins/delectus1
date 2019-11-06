(ns delectus-api-server.errors)

(defn error-if-empty [value message context]
  (if (nil? value)
    (throw (ex-info message (ex-info (merge context {:error-signaled-by 'error-if-empty}))))))

(defn error-if-nil [value message context]
  (if (nil? value)
    (throw (ex-info message (ex-info (merge context {:error-signaled-by 'error-if-nil}))))))

(defn error-if-not [test-val message context]
  (if-not test-val
    (throw (ex-info message (ex-info (merge context {:error-signaled-by 'error-if-not}))))))
