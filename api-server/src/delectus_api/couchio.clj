(ns delectus-api.couchio
  (:require
   [clojure.edn :as edn]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]])
  (:import
   (com.couchbase.client.core CouchbaseException)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))


;;; =====================================================================
;;; Couchbase error handling
;;; =====================================================================

(defmacro with-couchbase-exceptions-rethrown [& forms]
  (let [exname (gensym)]
    `(try
       (do ~@forms)
       (catch CouchbaseException ~exname
         (throw (ex-info "Couchbase error"
                         {:cause :couchbase-exception
                          :exception-object ~exname})))
       (catch Exception ~exname
         (throw (ex-info "Unrecognized error"
                         {:cause :exception
                          :exception-object ~exname}))))))

;;; =====================================================================
;;; JsonObjects
;;; =====================================================================

;;; accessors
;;; ---------------------------------------------------------------------

(defn ensure-json-object [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  obj)

(defn json-object-attribute [obj attribute-name]
  (ensure-json-object obj)
  (.get obj attribute-name))

;;; predicates
;;; ---------------------------------------------------------------------

(defn json-object-type? [obj type-string]
  (= type-string (json-object-attribute obj +type-attribute+)))

(defn json-object-owner? [obj ownerid]
  (= ownerid (json-object-attribute obj +owner-id-attribute+)))

;;; =====================================================================
;;; JsonDocuments
;;; =====================================================================

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; =====================================================================
;;; Searches and queries
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; Finding objects with specific attribute values
;;; ---------------------------------------------------------------------

(defn make-object-matchers [matchers-map]
  (let [ks (keys matchers-map)]
    (map #(str "`" % "` = \"" (get matchers-map %) "\"")
         ks)))

;;; (make-object-matchers {})

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
;;; (make-object-selector (config/delectus-users-bucket) ["id" "type"] {})
;;; (make-object-selector (config/delectus-users-bucket) ["id" "type"] {"type" +delectus-list-document-type+ "id" "FOO!"})

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

;;; (def $objs (find-objects (config/delectus-content-bucket) [] {"type" +collection-type+}))
;;; (def $objs (find-objects (config/delectus-content-bucket) ["name"] {"type" +list-type+}))

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
          value (.content result 0)]
      (if value
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

(defn document-path-exists [bucket documentid key-path]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.lookupIn bucket documentid)
          exists (.exists lookup (into-array [key-path]))
          result (.execute exists)]
      (.content result 0))))

;;; (document-path-exists (config/delectus-content-bucket) "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "columns.0.name")
;;; (document-path-exists (config/delectus-content-bucket) "8a61bdbc-3910-4257-afec-9ba34ac3fa45" "NOPE!")

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

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn user-exists? [userid]
  (and (id-exists? (config/delectus-users-bucket) userid)
       (= +user-type+
          (get-object-attribute (config/delectus-users-bucket)
                                userid +type-attribute+))))

(defn get-user [userid]
  (or (and userid
           (let [candidate (get-document (config/delectus-users-bucket) userid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (json-object-type? obj +user-type+)
                      obj
                      nil)))))
      nil))

;;; finding registered users
;;; ---------------------------------------------------------------------

(defn email->user [email]
  (let [found (find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (first found))))

(defn email->userid [email]
  (let [found (find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (.get (first found) +id-attribute+))))

(defn id->user [userid]
  (get-user userid))

;;; Collections
;;; ---------------------------------------------------------------------

(defn collection-exists? [collectionid]
  (and (id-exists? (config/delectus-content-bucket) collectionid)
       (= +collection-type+
          (get-object-attribute (config/delectus-content-bucket)
                                collectionid +type-attribute+))))

(defn get-collection [collectionid]
  (or (and collectionid
           (let [candidate (get-document (config/delectus-content-bucket) collectionid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (json-object-type? obj +collection-type+)
                      obj
                      nil)))))
      nil))

;;; Lists
;;; ---------------------------------------------------------------------

(defn list-exists? [listid]
  (and (id-exists? (config/delectus-content-bucket) listid)
       (= +list-type+
          (get-object-attribute (config/delectus-content-bucket)
                                listid +type-attribute+))))

(defn get-list [listid]
  (or (and listid
           (let [candidate (get-document (config/delectus-content-bucket) listid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (json-object-type? obj +list-type+)
                      obj
                      nil)))))
      nil))


