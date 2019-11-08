(ns delectus-api-server.export
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :refer :all]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors])
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; tools for exporting Couchbase Delectus data to files

(defn export-delectus-users [out-file]
  (let [bucket (config/delectus-users-bucket)
        selector (couchio/make-object-selector bucket [] {+type-attribute+ +delectus-user-document-type+})
        results (.query bucket (N1qlQuery/simple selector))
        objects (map #(get (.toMap (.value %))
                           +delectus-users-bucket-name+)
                     results)]
    (spit out-file "" :append false)
    (with-open [out (io/writer out-file)]
      (doseq [obj objects]
        (pprint obj out)))))

;;; (time (export-delectus-users "/home/mikel/Desktop/delectus-users.edn"))


(defn export-delectus-collections [out-file]
  (let [bucket (config/delectus-content-bucket)
        selector (couchio/make-object-selector bucket [] {+type-attribute+ +delectus-collection-document-type+})
        results (.query bucket (N1qlQuery/simple selector))
        objects (map #(get (.toMap (.value %))
                           +delectus-content-bucket-name+)
                     results)]
    (spit out-file "" :append false)
    (with-open [out (io/writer out-file)]
      (doseq [obj objects]
        (pprint obj out)))))

;;; (time (export-delectus-collections "/home/mikel/Desktop/delectus-collections.edn"))

(defn export-delectus-lists [out-file]
  (let [bucket (config/delectus-content-bucket)
        selector (couchio/make-object-selector bucket [] {+type-attribute+ +delectus-list-document-type+})
        results (.query bucket (N1qlQuery/simple selector))
        objects (map #(get (.toMap (.value %))
                           +delectus-content-bucket-name+)
                     results)]
    (spit out-file "" :append false)
    (with-open [out (io/writer out-file)]
      (doseq [obj objects]
        (pprint obj out)))))

;;; (time (export-delectus-lists "/home/mikel/Desktop/delectus-lists.edn"))
