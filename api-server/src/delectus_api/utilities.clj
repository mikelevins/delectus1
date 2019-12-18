(ns delectus-api.utilities)

;;; contains-value? [a-map pred] => Boolean
;;; ---------------------------------------------------------------------

(defn contains-value? [a-map pred]
  (some (fn [kv]
          (and (pred (val kv))
               (val kv)))
        a-map))

