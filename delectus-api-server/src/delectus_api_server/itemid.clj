(ns delectus-api-server.itemid)

(defn first-itemid [] "0")

(defn next-itemid [id]
  (str (+ 1 (Long/parseLong id))))

;;; (next-itemid (first-itemid))

(defn id->int [id]
  (Long/parseLong id))

(defn itemid< [id1 id2]
  (< (id->int id1)
     (id->int id2)))


;;; (itemid< "1" "5")
;;; (itemid< "5" "1")
;;; (sort itemid< ["123" "12" "516" "0" "7" "1024"])
;;; 

;;; for testing

(defn random-ints [how-many limit]
  (take how-many (repeatedly #(rand-int limit))))

;;; (def $rands (random-ints 10000 100))
;;; (def $strs (into [] (map str $rands)))
;;; (count $strs)
;;; (def $sorted (time (sort itemid< $strs)))
;;; (take 10 $sorted)
;;; (drop 990 $sorted)
;;; (take 10 (drop 400 $sorted))
