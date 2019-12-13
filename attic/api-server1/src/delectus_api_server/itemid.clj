(ns delectus-api-server.itemid)

;;; for testing

(defn random-ints [how-many limit]
  (take how-many (repeatedly #(rand-int limit))))


(defn first-itemid [] "0")

(defn inc-itemid [id]
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

(defn itemid-max [id1 id2]
  (if (itemid< id1 id2)
    id2
    id1))

;;; (def $rands (random-ints 100000 100))
;;; (def $strs (into [] (map str $rands)))
;;; (count $strs)
;;; (time (reduce itemid-max $strs))

(defn next-itemid [ids]
  (let [len (count ids)]
    (cond
      (< len 1) (first-itemid)
      (= len 1) (inc-itemid (first ids))
      :else (let [maxid (reduce itemid-max ids)]
              (inc-itemid maxid)))))

;;; (next-itemid [])
;;; (next-itemid ["100"])
;;; (def $rands (random-ints 100000 100))
;;; (def $strs (into [] (map str $rands)))
;;; (count $strs)
;;; (time (next-itemid $strs))

;;; (def $sorted (time (sort itemid< $strs)))
;;; (take 10 $sorted)
;;; (drop 990 $sorted)
;;; (take 10 (drop 40000 $sorted))
