(ns delectus-api.utilities)

;;; contains-value? [a-map pred] => Boolean
;;; ---------------------------------------------------------------------

(defn contains-value? [a-map pred]
  (some (fn [kv]
          (and (pred (val kv))
               (val kv)))
        a-map))

;;; string utilities
;;; ---------------------------------------------------------------------

(defn string< [left right]
  (= -1 (compare left right)))

(defn string= [left right]
  (= 0 (compare left right)))

(defn string> [left right]
  (= 1 (compare left right)))

(defn string<= [left right]
  (or (string= left right)
      (string< left right)))

(defn string>= [left right]
  (or (string= left right)
      (string> left right)))
