(ns delectus-api-server.couchbase.io
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.data.csv :refer [read-csv]]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.io :as couchbase-io]
            [delectus-api-server.couchbase.marshal :as marshal]
            [delectus-api-server.couchbase.utilities :as couch-utils])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))

;;; ---------------------------------------------------------------------
;;; fetching documents and objects
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

;;; ---------------------------------------------------------------------
;;; searching for objects
;;; ---------------------------------------------------------------------

(defn make-where-clause [property-map]
  (if (empty? property-map)
    ""
    (let [property-matches (map #(let [val (get property-map %)]
                                   (cl-format nil "`~A` = ~S"
                                              (marshal/make-couchable %)
                                              (marshal/make-couchable val)))
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

;;; (time (def $all (find-objects (config/delectus-users-bucket) {})))


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
;;; (:id (first $routes))
;;; (:document-key (first $routes))

(defn find-object-ids [bucket properties
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
        select-expression (cl-format nil "SELECT meta(doc).id AS docid from `~A` doc ~A ~A ~A ~A"
                                     bucket-name where-clause order-clause limit-clause offset-clause)
        results (.query bucket (N1qlQuery/simple select-expression))
        result-vals (map #(.value %) results)]
    (map #(.getString % "docid") result-vals)))

;;; (time (def $airline-ids (find-object-ids (config/travel-sample-bucket) {"type" "airline"})))
;;; (count $airline-ids)
;;; (nth $airline-ids 25)

;;; (time (def $user-ids (find-object-ids (config/delectus-users-bucket) {"type" "delectus_user"})))
;;; (count $user-ids)
;;; (nth $user-ids 1)

(defn ids-of-type [bucket type-name]
  (let [bucket-name (.name bucket)
        select-expression (cl-format nil "SELECT meta(doc).id AS docid from `~A` doc WHERE `type` = \"~A\""
                                     bucket-name type-name)
        results (.query bucket (N1qlQuery/simple select-expression))
        result-vals (map #(.value %) results)]
    (map #(.getString % "docid") result-vals)))

;;; (time (def $route-ids (ids-of-type (config/travel-sample-bucket) "route")))
;;; (nth $route-ids 0)

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
;;; creating and updating documents
;;; ---------------------------------------------------------------------

(defn create-document! [bucket document-key document-map]
  (let [key (or document-key (makeid))
        json-doc (marshal/to-json-document document-map document-key)]
    (.insert bucket json-doc)
    key))

(defn delete-document! [bucket document-key]
  (.remove bucket document-key))

(defn update-document! [bucket document-key new-document-map]
  (let [old-document-map (marshal/to-map (get-object bucket document-key))
        updated-document-map (if old-document-map
                               (merge old-document-map new-document-map)
                               new-document-map)
        json-doc (marshal/to-json-document updated-document-map document-key)]
    (.upsert bucket json-doc)
    document-key))

;;; ---------------------------------------------------------------------
;;; importing csv
;;; ---------------------------------------------------------------------

(defn read-csv-file [path]
  (with-open [reader (io/reader path)]
    (into []
          (doall ; forces read of all records, so the stream doesn't have to stay open in order to get them
           (read-csv reader)))))

;;; (def $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.csv")
;;; (time (def $movies (read-csv-file $movies-path)))
;;; (class (first $movies))
;;; (second $movies)
;;; (class $movies)
;;; (class (into [] (rest $movies)))

(defn csv-file->JsonDocument [name id type path]
  (let [csv-data (read-csv-file path)
        id (or id (makeid))
        column-labels (first csv-data)
        rows-data (into [] (rest csv-data))]
    (marshal/to-json-document {:name name
                               :id id
                               :type type
                               :columns column-labels
                               :rows rows-data}
                              id)))

;;; (def $movies-doc (csv-file->JsonDocument "Mom's Movies" nil "delectus_list" $movies-path ))

(defn import-csv-file [path owner list-name id]
  (let [csv-data (read-csv-file path)
        id (or id (makeid))
        column-labels (first csv-data)
        rows-data (into [] (rest csv-data))
        docmap {:name list-name
                :id id
                :type "delectus_list"
                :columns column-labels
                :rows rows-data}
        bucket (config/delectus-content-bucket)]
    (couchbase-io/create-document! bucket id docmap)
    id))

(defn lookup-in [bucket id]
  (.lookupIn bucket id))

(defn get-xattrs [bucket id]
  (let [found-doc (lookup-in bucket id)]
    (marshal/to-map
     (.content (.execute (.get found-doc "$document"
                               (.xattr (new SubdocOptionsBuilder)
                                       true)))
               0))))

(defn size-in-bytes [bucket id]
  (:value_bytes (get-xattrs bucket id)))


