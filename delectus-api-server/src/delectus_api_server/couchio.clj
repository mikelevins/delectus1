(ns delectus-api-server.couchio
  (:require
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :refer :all]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]])
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))

;;; =====================================================================
;;; document and object helpers
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; JsonObject
;;; ---------------------------------------------------------------------

;;; accessors
;;; ---------------------------------------------------------------------

(defn ensure-json-object [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  obj)

(defn json-object-attribute [obj attribute-name]
  (ensure-json-object obj)
  (.get obj attribute-name))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (json-object-items (get-collection $defaultid))

;;; predicates
;;; ---------------------------------------------------------------------

(defn json-object-type? [obj type-string]
  (= type-string (json-object-attribute obj +type-attribute+)))

(defn json-object-owner? [obj ownerid]
  (= ownerid (json-object-attribute obj +owner-id-attribute+)))

;;; (def $fred (make-json-object {"name" "Fred" "age" 35}))
;;; (itemizing-json-object? $fred)
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (itemizing-json-object? (get-collection $defaultid))

;;; constructors
;;; ---------------------------------------------------------------------

(defn make-json-object [object-map]
  (JsonObject/from object-map))

;;; (make-json-object {"name" "Fred" "age" 35})
;;; (make-json-object {"name" "Fred" "age" 35 "things" {}})

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-list-document :name "Random stuff" :owner-id $mikelid)


;;; ---------------------------------------------------------------------
;;; JsonDocument
;;; ---------------------------------------------------------------------

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; (make-json-document "foo_document" {"name" "Fred" "age" 35})
;;; (make-json-document "bar_document" {"name" "Fred" "age" 35 "things" {}})

;;; =====================================================================
;;; Fetching and storing documents by id
;;; ---------------------------------------------------------------------

;;; generic JsonDocuments
;;; ---------------------------------------------------------------------

(defn id-exists? [bucket docid]
  (.exists bucket docid))

;;; (def $bucket (config/delectus-users-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (id-exists? $bucket $mikelid)
;;; (id-exists? $bucket "NOPE!")

(defn get-document [bucket docid]
  (.get bucket docid))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (def $docid (.get (delectus-api-server.api/collection-named $mikelid  "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))

;;; Users
;;; ---------------------------------------------------------------------

(defn get-user [userid]
  (or (and userid
           (let [candidate (get-document (config/delectus-users-bucket) userid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj +user-type+)
                    obj))))
      nil))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user $mikelid)
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-user $defaultid)
;;; (def $nopeid nil)
;;; (get-user $nopeid)

;;; Collections
;;; ---------------------------------------------------------------------

(defn get-collection [collectionid]
  (or (and collectionid
           (let [candidate (get-document (config/delectus-content-bucket) collectionid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj +collection-type+)
                    obj))))
      nil))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection $defaultid)
;;; (def $bucket (config/delectus-content-bucket))
;;; (.content (.execute (.get (.lookupIn $bucket $defaultid) (into-array ["lists"]))) 0)
;;; (.content (.execute (.get (.lookupIn $bucket $defaultid) (into-array ["NOPE"]))) 0)

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-collection $mikelid)
;;; (def $nopeid nil)
;;; (get-collection $nopeid)

;;; Lists
;;; ---------------------------------------------------------------------

(defn get-list [listid]
  (or (and listid
           (let [candidate (get-document (config/delectus-content-bucket) listid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj +list-type+)
                    obj))))
      nil))

;;; (def $thingsid "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list $thingsid)
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-list $mikelid)
;;; (def $nopeid nil)
;;; (get-list $nopeid)


;;; =====================================================================
;;; Couchbase subdocument access
;;; =====================================================================
;;; Subdocument access enables us to fetch and store ojbject properties
;;; without fetching whole objects

;;; ---------------------------------------------------------------------
;;; common general accessors
;;; ---------------------------------------------------------------------

(defn object-attribute-exists? [bucket objectid attribute-name]
  (let [lookup (.exists (.lookupIn bucket objectid) (into-array [attribute-name]))
        result (.execute lookup)]
    (.content result 0)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (object-attribute-exists? (config/delectus-users-bucket) $mikelid "id")
;;; (object-attribute-exists? (config/delectus-users-bucket) $mikelid "nope")

(defn ensure-object-attribute-exists [bucket object-id attribute-name]
  (errors/error-if-not (object-attribute-exists? bucket object-id attribute-name)
                       "No such attribute"
                       {:bucket-name (.name bucket)
                        :object-id object-id
                        :attribute-name attribute-name})
  object-id)

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-object-attribute-exists (config/delectus-users-bucket) $mikelid "id")
;;; (ensure-object-attribute-exists (config/delectus-users-bucket) $mikelid "nope")

(defn get-object-attribute [bucket objectid attribute-name]
  (ensure-object-attribute-exists bucket objectid attribute-name)
  (let [lookup (.get (.lookupIn bucket objectid) (into-array [attribute-name]))
        result (.execute lookup)]
    (.content result 0)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-object-attribute (config/delectus-users-bucket) $mikelid "id")
;;; (get-object-attribute (config/delectus-users-bucket) $mikelid "nope")

;;; set the attribute, but only if it already exists
(defn update-object-attribute! [bucket objectid attribute-name value]
  (ensure-object-attribute-exists bucket objectid attribute-name)
  (let [mutator (.upsert (.mutateIn bucket objectid) attribute-name value)]
    (.execute mutator)
    value))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-object-attribute (config/delectus-users-bucket) $mikelid "name")
;;; (update-object-attribute! (config/delectus-users-bucket) $mikelid "name" "mikel evins")
;;; (update-object-attribute! (config/delectus-users-bucket) $mikelid "NOT-PRESENT" "FAIL")

;;; set the attribute, adding it to the object if it's not present
(defn upsert-object-attribute! [bucket objectid attribute-name value]
  (let [mutator (.upsert (.mutateIn bucket objectid) attribute-name value)]
    (.execute mutator)
    value))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-object-attribute (config/delectus-users-bucket) $mikelid "age")
;;; (upsert-object-attribute! (config/delectus-users-bucket) $mikelid "age" 59)

(defn get-object-type [bucket objectid]
  (get-object-attribute bucket objectid +type-attribute+))

;;; =====================================================================
;;; N1QL queries
;;; =====================================================================
;;; Searching for objects that match patterns

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
  (let [selector (make-object-selector bucket keys matching)
        bucket-name (.name bucket)
        results (.query bucket (N1qlQuery/simple selector))]
    (if (empty? keys)
      ;; empty keys generate a SELECT *
      ;; SELECT * returns each result wrapped in a map like this: {bucket-name found-object}
      (map #(.get (.value %) bucket-name) results)
      (map #(.value %) results))))

;;; (def $objs (find-objects (config/delectus-content-bucket) [] {"type" +delectus-list-document-type+}))
;;; (def $objs (find-objects (config/delectus-content-bucket) ["name"] {"type" +delectus-list-document-type+}))

;;; =====================================================================
;;; couchio errors
;;; =====================================================================

(defn error-if-no-such-id [message bucket id]
  (if-not (id-exists? bucket id)
    (throw (ex-info message
                    {:id id
                     :bucket (.name bucket)
                     :error-signaled-by 'error-if-no-such-id}))))

(defn error-if-wrong-type [message couch-object type-name]
  (if-not (= type-name (.get couch-object +type-attribute+))
    (throw (ex-info message
                    {:object-id (.get couch-object +id-attribute+) 
                     :expected-type type-name
                     :found-type (.get couch-object +type-attribute+)
                     :error-signaled-by 'error-if-wrong-type}))))

(defn error-if-wrong-owner [message couch-object owner-id]
  (if-not (= owner-id (.get couch-object +owner-id-attribute+))
    (throw (ex-info message
                    {:object-id (.get couch-object +id-attribute+) 
                     :expected-owner owner-id
                     :found-owner (.get couch-object +owner-id-attribute+)
                     :error-signaled-by 'error-if-wrong-owner}))))

(defn error-if-id-exists [id]
  (let [found (get-document (config/delectus-content-bucket) id)]
    (if found
      (throw (ex-info "An object with this ID already exists"
                      {:id id
                       :bucket "delectus-content-bucket"
                       :error-signaled-by 'error-if-collection-id-exists})))))
