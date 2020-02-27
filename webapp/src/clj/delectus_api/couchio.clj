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
  (and (instance? JsonObject obj)
       (= type-string (json-object-attribute obj +type-key+))))

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

(defn make-object-selector [bucket keys match offset limit]
  (let [bucket-name (.name bucket)
        key-expression (if (empty? keys)
                         "*"
                         (clojure.string/join "," (map str keys)))
        matchers (make-object-matchers match)
        where-clause (if (empty? matchers)
                       ""
                       (str "WHERE "
                            (clojure.string/join " AND " matchers)))
        offset-clause (if (nil? offset) "" (cl-format nil " OFFSET ~A " offset))
        limit-clause (if (nil? limit) "" (cl-format nil " LIMIT ~A " limit))
        terminator ";"
        selector (str "SELECT " key-expression " FROM `" bucket-name "` " where-clause
                      offset-clause limit-clause terminator)]
    selector))

;;; (make-object-selector (config/delectus-content-bucket) [] {} 0 100)
;;; (make-object-selector (config/delectus-content-bucket) [] {} nil nil)
;;; (make-object-selector (config/delectus-content-bucket) ["id" +type-key+] {} 0 100)
;;; (make-object-selector (config/delectus-content-bucket) ["id" +type-key+] {+type-key+ +list-type+} 0 100)
;;; (make-object-selector (config/delectus-content-bucket) ["id" +type-key+] {+id-key+ "FOO" +collection-key+ nil} 0 100)
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-object-selector (config/delectus-content-bucket) [] {+type-key+ +collection-type+ +owner-key+ $mikelid} 0 100)
;;; (make-object-selector (config/delectus-content-bucket) [] {+type-key+ +list-type+ +name-key+ "Zipcodes" +owner-key+ $mikelid} 0 100)



(defn find-objects [bucket
                    & {:keys [keys match offset limit]
                       :or {keys []
                            match nil
                            offset (:delectus-default-find-offset (config/delectus-configuration))
                            limit (:delectus-default-find-limit (config/delectus-configuration))}}]
  (with-couchbase-exceptions-rethrown
    (let [selector (make-object-selector bucket keys match offset limit)
          bucket-name (.name bucket)
          results (.query bucket (N1qlQuery/simple selector))]
      (if (empty? keys)
        ;; empty keys generate a SELECT *
        ;; SELECT * returns each result wrapped in a map like this: {bucket-name found-object}
        (map #(.get (.value %) bucket-name) results)
        (map #(.value %) results)))))

;;; (def $found (find-objects (config/delectus-content-bucket) :keys [] :match {+type-key+ +item-type+}))
;;; (count $found)
;;; (nth $found 0)

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

