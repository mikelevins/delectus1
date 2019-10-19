(ns delectus-api-server.couchbase.io
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.delectus.users :as users]
            [delectus-api-server.couchbase.utilities :refer [for-couchbase map->JsonObject]])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; reading and writing data stored in Couchbase
;;; ---------------------------------------------------------------------

;;; takes constant time, regardless of the number of objects in the db

(defn get-document [bucket id]
  (.get bucket id))

(defn get-object [bucket id]
  (.content (get-document bucket id)))

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

;;; takes time proportional to the number of documents in the db
;;; faster if the properties are indexed
(defn find-objects [bucket properties]
  (let [bucket-name (.name bucket)
        property-matches (map #(let [val (get properties %)]
                                 (cl-format nil "`~A` = ~S"
                                            (for-couchbase %)(for-couchbase val)))
                              (keys properties))
        property-clause (cl-format nil "~{~A~^ AND ~}" property-matches)
        select-expression (cl-format nil "SELECT * from `~A` WHERE ~A"
                                     bucket-name property-clause)
        results (.query bucket (N1qlQuery/simple select-expression))]
    (map #(object->map (.value %)) results)))

;;; (time (def $airlines (find-objects (config/travel-sample-bucket) {"type" "airline" "id" 10})))
;;; (time (def $airlines (find-objects (config/travel-sample-bucket) {"type" "airline"})))
;;; (class (first $airlines))
;;; (time (def $routes (find-objects (config/travel-sample-bucket) {"type" "route" "id" 10000})))
;;; (time (def $routes (find-objects (config/travel-sample-bucket) {"type" "route"})))
;;; (count $routes)
;;; (first $routes)

(defn count-objects [bucket properties]
  (let [bucket-name (.name bucket)]
    (if (empty? properties)
      (let [select-expression (cl-format nil "SELECT COUNT(*) AS count from `~A`" bucket-name)
            results (.query bucket (N1qlQuery/simple select-expression))]
        (.getLong (.value (first (.allRows results)))
                  "count"))
      (let [property-matches (map #(let [val (get properties %)]
                                     (cl-format nil "`~A` = ~S"
                                                (for-couchbase %)(for-couchbase val)))
                                  (keys properties))
            property-clause (cl-format nil "~{~A~^ AND ~}" property-matches)
            select-expression (cl-format nil "SELECT COUNT(*) AS count from `~A` WHERE ~A"
                                         bucket-name property-clause)
            results (.query bucket (N1qlQuery/simple select-expression))]
        (.getLong (.value (first (.allRows results)))
                  "count")))))

;;; (time (count-objects (config/travel-sample-bucket) {}))
;;; (time (count-objects (config/travel-sample-bucket) {"type" "route"}))
;;; not constant-time, but pretty fast
;;; counting 188 airlines: 9.7 milliseconds 
;;; counting 1968 airports: 21.1 milliseconds 
;;; counting 24024 routes: 94.2 milliseconds 
;;; counting 31695 documents: 17.6 milliseconds 

