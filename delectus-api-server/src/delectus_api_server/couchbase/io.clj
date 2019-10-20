(ns delectus-api-server.couchbase.io
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.data.csv :refer [read-csv]]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.delectus.users :as users]
            [delectus-api-server.couchbase.utilities :refer [for-couchbase]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; fetching data stored in Couchbase
;;; ---------------------------------------------------------------------

;;; takes constant time, regardless of the number of objects in the db

(defn get-document [bucket id]
  (.get bucket id))

(defn get-object [bucket id]
  (let [doc (get-document bucket id)]
    (if doc
      (.content doc)
      nil)))

;;; (time (get-document (config/travel-sample-bucket) "airline_10"))
;;; (time (get-object (config/travel-sample-bucket) "airline_10"))
;;; (time (get-document (config/travel-sample-bucket) "route_10000"))
;;; (time (get-document (config/travel-sample-bucket) "NOPE!"))

(defn document-key [document]
  (.id document))

;;; (document-key (get-document (config/travel-sample-bucket) "airline_10"))
;;; (document-key (get-document (config/travel-sample-bucket) "route_10000"))

(defn object->map [object]
  (json/read-json (.toString object)
                  true))

;;; (object->map (get-object (config/travel-sample-bucket) "airline_10"))

(defn document->map [document]
  (object->map (.content document)))

;;; (document->map (get-document (config/travel-sample-bucket) "airline_10"))

;;; ---------------------------------------------------------------------
;;; searching for objects stored in Couchbase
;;; ---------------------------------------------------------------------

(defn make-where-clause [property-map]
  (if (empty? property-map)
    ""
    (let [property-matches (map #(let [val (get property-map %)]
                                   (cl-format nil "`~A` = ~S"
                                              (for-couchbase %)
                                              (for-couchbase val)))
                                (keys property-map))]
      (cl-format nil "WHERE ~{~A~^ AND ~}" property-matches))))

;;; (make-where-clause {})
;;; (make-where-clause {:type "airport"})
;;; (make-where-clause {"type" "airport"})
;;; (make-where-clause {:id 10})
;;; (make-where-clause {"type" "airport" "id" 10})

(defn make-order-clause [order-by direction]
  (if order-by
    (let [dir (if direction
                (cond (= direction :ascending) "ASC"
                      (= direction :descending) "DESC"
                      :else (throw (ex-info "Invalid direction parameter" {:value direction :context "ORDER BY"})))
                "ASC")]
      (cl-format nil "ORDER BY `~A` ~A" order-by dir))
    ""))

;;; (make-order-clause nil nil)
;;; (make-order-clause "city" nil)
;;; (make-order-clause "city" :ascending)
;;; (make-order-clause "city" :descending)

;;; takes time proportional to the number of documents in the db
;;; faster if the properties are indexed

(defn find-objects [bucket properties
                    & {:keys [order-by direction limit offset]
                       :or {order-by nil
                            direction :ascending
                            limit nil
                            offset nil}}]
  (let [bucket-name (.name bucket)
        order-clause (make-order-clause order-by direction)
        where-clause (make-where-clause properties)
        limit-clause (if limit (cl-format nil "LIMIT ~A" limit) "")
        offset-clause (if offset (cl-format nil "OFFSET ~A" offset) "")
        select-expression (cl-format nil "SELECT *, meta(doc).id AS docid from `~A` doc ~A ~A ~A ~A"
                                     bucket-name where-clause order-clause limit-clause offset-clause)
        results (.query bucket (N1qlQuery/simple select-expression))
        result-vals (map #(.value %) results)
        result-strings (map #(.toString %) result-vals)]
    (map #(let [obj (json/read-json %)
                doc (:doc obj)
                docid (:docid obj)]
            (merge doc {:document-key docid}))
         result-strings)))

;;; (time (def $all (find-objects (config/travel-sample-bucket) {})))
;;; (time (def $airlines (find-objects (config/travel-sample-bucket) {"type" "airline" "id" 10})))
;;; (time (def $airlines (find-objects (config/travel-sample-bucket) {"type" "airline"})))
;;; (time (def $airlines (find-objects (config/travel-sample-bucket) {"type" "airline"} :order-by "callsign")))
;;; (count $airlines)
;;; (nth $airlines 25)

;;; (time (def $routes (find-objects (config/travel-sample-bucket) {"type" "route" "id" 10000})))
;;; (time (def $routes (find-objects (config/travel-sample-bucket) {"type" "route"})))
;;; (time (def $routes (find-objects (config/travel-sample-bucket) {"type" "route"} :limit 10 :offset 20000)))
;;; (count $routes)
;;; (first $routes)

(defn count-objects [bucket properties]
  (let [bucket-name (.name bucket)
        where-clause (make-where-clause properties)
        select-expression (cl-format nil "SELECT COUNT(*) AS count from `~A` ~A"
                                     bucket-name where-clause)
        results (.query bucket (N1qlQuery/simple select-expression))]
    (.getLong (.value (first (.allRows results)))
              "count")))

;;; (time (count-objects (config/travel-sample-bucket) {}))
;;; (time (count-objects (config/travel-sample-bucket) {"type" "route"}))
;;; (time (count-objects (config/travel-sample-bucket) {"type" "airline"}))
;;; not constant-time, but pretty fast
;;; counting 188 airlines: 9.7 milliseconds 
;;; counting 1968 airports: 21.1 milliseconds 
;;; counting 24024 routes: 94.2 milliseconds 
;;; counting 31695 documents: 17.6 milliseconds 

;;; ---------------------------------------------------------------------
;;; creating documents in Couchbase
;;; ---------------------------------------------------------------------

(defn create-document [bucket document-key document-map]
  (let [key (or document-key (makeid))
        json-object (for-couchbase document-map)
        json-doc (JsonDocument/create key json-object)]
    (.insert bucket json-doc)
    key))

;;; (def $docid (create-document (config/delectus-bucket) (makeid) {:name "Fred" :type "cartoon"}))
;;; (object->map (get-object (config/delectus-bucket) $docid))

(defn delete-document [bucket document-key]
  (.remove bucket document-key))

;;; (delete-document (config/delectus-bucket) $docid)
;;; (get-object (config/delectus-bucket) $docid)

;;; ---------------------------------------------------------------------
;;; reading csv
;;; ---------------------------------------------------------------------

(defn read-csv-file [path]
  (with-open [reader (io/reader path)]
    (doall
     (read-csv reader))))

;;; (def $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.csv")
;;; (time (def $movies (read-csv-file $movies-path)))
;;; (first $movies)
;;; (second $movies)

;;; (def $zipcodes-path "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (time (def $zipcodes (read-csv-file $zipcodes-path)))
;;; (count $zipcodes)
;;; (first $zipcodes)
;;; (nth $zipcodes 10000)
