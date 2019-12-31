(ns delectus-api.couchio
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.pprint :refer [cl-format]]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]])
  (:import
   (com.couchbase.client.core CouchbaseException)
   (com.couchbase.client.java CouchbaseBucket)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))


;;; =====================================================================
;;; Couchbase error handling
;;; =====================================================================

(defmacro with-couchbase-exceptions-rethrown [& forms]
  `(try
     (do ~@forms)
     (catch CouchbaseException couchexc#
       (throw (ex-info "Couchbase error"
                       {:cause :couchbase-exception
                        :exception-object couchexc#})))
     (catch Exception exc#
       (throw (ex-info "Unrecognized error"
                       {:cause :exception
                        :exception-object exc#})))))

;;; =====================================================================
;;; JsonObjects
;;; =====================================================================

;;; accessors
;;; ---------------------------------------------------------------------

(defn make-json-object [object-map]
  (JsonObject/from object-map))

(defn ensure-json-object [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  obj)

(defn json-object-attribute [obj attribute-name]
  (ensure-json-object obj)
  (.get obj attribute-name))

;;; predicates
;;; ---------------------------------------------------------------------

(defn json-object-type? [obj type-string]
  (= type-string (json-object-attribute obj +type-key+)))

(defn json-object-owner? [obj ownerid]
  (= ownerid (json-object-attribute obj +owner-key+)))

;;; =====================================================================
;;; JsonDocuments
;;; =====================================================================

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

(defn remove-document! [bucket docid]
  (.remove bucket docid))

;;; =====================================================================
;;; Searches and queries
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; Finding objects with specific attribute values
;;; ---------------------------------------------------------------------

(defn make-object-matchers [matchers-map]
  (map (fn [e]
         (let [k (key e)
               v (val e)]
           (if (nil? v)
             (cl-format nil "`~A` IS NULL" k)
             (cl-format nil "`~A` = \"~A\"" k v))))
       matchers-map))

;;; (make-object-matchers {})
;;; (make-object-matchers {+id-key+ "FOO" +collection-key+ nil})

(defn make-object-selector [bucket keys matching]
  (let [bucket-name (.name bucket)
        key-expression (if (empty? keys)
                         "*"
                         (clojure.string/join "," (map str keys)))
        matchers (make-object-matchers matching)
        where-clause (if (empty? matchers)
                       ";"
                       (str "WHERE "
                            (clojure.string/join " AND " matchers)
                            ";"))
        selector (str "SELECT " key-expression " FROM `" bucket-name "` " where-clause)]
    selector))

;;; (make-object-selector (config/delectus-users-bucket) [] {})
;;; (make-object-selector (config/delectus-users-bucket) ["id" +type-key+] {})
;;; (make-object-selector (config/delectus-users-bucket) ["id" +type-key+] {+type-key+ +list-type+})
;;; (make-object-selector (config/delectus-users-bucket) ["id" +type-key+] {+id-key+ "FOO" +collection-key+ nil})

(defn make-object-counter [bucket matching]
  (let [bucket-name (.name bucket)
        matchers (make-object-matchers matching)
        where-clause (if (empty? matchers)
                       ";"
                       (str "WHERE "
                            (clojure.string/join " AND " matchers)
                            ";"))
        selector (str "SELECT COUNT(*) FROM `" bucket-name "` " where-clause)]
    selector))

;;; (make-object-counter (config/delectus-users-bucket) {+id-key+ "FOO" +collection-key+ nil})

(defn find-objects [bucket keys matching]
  (with-couchbase-exceptions-rethrown
    (let [selector (make-object-selector bucket keys matching)
          bucket-name (.name bucket)
          results (.query bucket (N1qlQuery/simple selector))]
      (if (empty? keys)
        ;; empty keys generate a SELECT *
        ;; SELECT * returns each result wrapped in a map like this: {bucket-name found-object}
        (map #(.get (.value %) bucket-name) results)
        (map #(.value %) results)))))

;;; (def $objs (find-objects (config/delectus-content-bucket) [] {+type-key+ +collection-type+}))
;;; (time (def $objs (find-objects (config/delectus-content-bucket) [] {+type-key+ +list-type+ +owner-key+ "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f" +collection-key+ nil})))
;;; (make-object-selector (config/delectus-content-bucket) [] {+type-key+ +list-type+ +owner-key+ "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f" +collection-key+ nil})

(defn count-objects [bucket matching]
  (with-couchbase-exceptions-rethrown
    (let [selector (make-object-counter bucket matching)
          bucket-name (.name bucket)
          results (.query bucket (N1qlQuery/simple selector))]
      (class results))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (time (count-objects (config/delectus-content-bucket) {+type-key+ +list-type+ +owner-key+ $mikelid +collection-key+ nil}))
;;; (time (count-objects (config/delectus-content-bucket) {+type-key+ +item-type+ +owner-key+ $mikelid +collection-key+ nil}))
;;; (time (count-objects (config/delectus-content-bucket) {+type-key+ +item-type+}))


;;; =====================================================================
;;; Couchbase accessors
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; Documents and attributes
;;; ---------------------------------------------------------------------

(defn id-exists? [bucket docid]
  (with-couchbase-exceptions-rethrown
    (.exists bucket docid)))

(defn get-document [bucket docid]
  (with-couchbase-exceptions-rethrown
    (.get bucket docid)))

(defn get-document-of-type [bucket docid type-name]
  (with-couchbase-exceptions-rethrown
    (if docid
      (let [doc (get-document bucket docid)]
        (if doc
          (let [obj (.content doc)]
            (if (json-object-type? obj type-name)
              doc
              nil))
          nil))
      nil)))

(defn get-object-of-type [bucket docid type-name]
  (let [doc (get-document-of-type bucket docid type-name)]
    (if doc
      (.content doc)
      nil)))

(defn object-attribute-exists? [bucket objectid attribute-name]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.exists (.lookupIn bucket objectid) (into-array [attribute-name]))
          result (.execute lookup)]
      (.content result 0))))

(defn get-object-attribute [bucket objectid attribute-name]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.get (.lookupIn bucket objectid) (into-array [attribute-name]))
          result (.execute lookup)]
      (.content result 0))))

;;; set the attribute, but only if it already exists
(defn update-object-attribute! [bucket objectid attribute-name value]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.exists (.lookupIn bucket objectid) (into-array [attribute-name]))
          result (.execute lookup)
          exists? (.content result 0)]
      (if exists?
        (let [mutator (.upsert (.mutateIn bucket objectid) attribute-name value)]
          (.execute mutator)
          value)
        (throw (CouchbaseException. "No such attribute"))))))

;;; set the attribute, adding it to the object if it's not present
(defn upsert-object-attribute! [bucket objectid attribute-name value]
  (with-couchbase-exceptions-rethrown
   (let [mutator (.upsert (.mutateIn bucket objectid) attribute-name value)]
    (.execute mutator)
    value)))

;;; ---------------------------------------------------------------------
;;; Key-value paths
;;; ---------------------------------------------------------------------

(defn document-path-exists? [bucket documentid key-path]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.lookupIn bucket documentid)
          exists (.exists lookup (into-array [key-path]))
          result (.execute exists)]
      (.content result 0))))

;;; (document-path-exists? (config/delectus-content-bucket) "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "columns.0.name")
;;; (document-path-exists? (config/delectus-content-bucket) "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "NOPE!")

(defn get-document-path [bucket documentid key-path]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.lookupIn bucket documentid)
          getter (.get lookup (into-array [key-path]))
          result (.execute getter)]
      (.content result 0))))

;;; (def $bucket (config/delectus-content-bucket))
;;; (get-document-path $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "columns.0.name")
;;; (get-document-path $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "NOPE!")

;;; set the attribute at the path, but only if it already exists
(defn update-document-path! [bucket documentid key-path value]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.exists (.lookupIn bucket documentid) (into-array [key-path]))
          result (.execute lookup)
          exists? (.content result 0)]
      (if exists?
        (let [mutator (.upsert (.mutateIn bucket documentid) key-path value)]
          (.execute mutator)
          value)
        (throw (CouchbaseException. "No such attribute"))))))

;;; (def $bucket (config/delectus-content-bucket))
;;; (update-document-path! $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "columns.0.name" "Item")
;;; (update-document-path! $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "NOPE!" "frob")
;;; (get-document-path $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "columns.0.name")

;;; set the attribute, adding it to the object if it's not present
(defn upsert-document-path! [bucket documentid key-path value]
  (with-couchbase-exceptions-rethrown
    (let [mutator (.upsert (.mutateIn bucket documentid) key-path value)]
      (.execute mutator)
      value)))

;;; (def $bucket (config/delectus-content-bucket))
;;; (upsert-document-path! $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "test_attr" "Testing...")
;;; (get-document-path $bucket "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "test_attr")

