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


;;; ---------------------------------------------------------------------
;;; Couchbase error handling
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; JsonDocument
;;; ---------------------------------------------------------------------

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; (make-json-document "foo_document" {"name" "Fred" "age" 35})
;;; (make-json-document "bar_document" {"name" "Fred" "age" 35 "things" {}})

;;; ---------------------------------------------------------------------
;;; N1QL queries
;;; ---------------------------------------------------------------------

;;; Find objects with specific attribute values
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

;;; ---------------------------------------------------------------------
;;; Fetching documents by id
;;; ---------------------------------------------------------------------

;;; generic JsonDocuments
;;; ---------------------------------------------------------------------

(defn id-exists? [bucket docid]
  (with-couchbase-exceptions-rethrown
    (.exists bucket docid)))

;;; (def $bucket (config/delectus-users-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (id-exists? $bucket $mikelid)
;;; (id-exists? $bucket "NOPE!")

(defn get-document [bucket docid]
  (with-couchbase-exceptions-rethrown
    (.get bucket docid)))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (def $docid (.get (delectus-api-server.api/collection-named $mikelid  "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))


;;; ---------------------------------------------------------------------
;;; Fetching and storing Couchbase document attributes
;;; ---------------------------------------------------------------------

(defn object-attribute-exists? [bucket objectid attribute-name]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.exists (.lookupIn bucket objectid) (into-array [attribute-name]))
          result (.execute lookup)]
      (.content result 0))))

;;; (def $thingsid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (get-list $thingsid)
;;; (time (object-attribute-exists? (config/delectus-content-bucket) $thingsid "name"))
;;; (time (object-attribute-exists? (config/delectus-content-bucket) $thingsid "NOPE!"))

(defn get-object-attribute [bucket objectid attribute-name]
  (with-couchbase-exceptions-rethrown
    (let [lookup (.get (.lookupIn bucket objectid) (into-array [attribute-name]))
          result (.execute lookup)]
      (.content result 0))))

;;; (def $thingsid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (get-list $thingsid)
;;; (time (get-object-attribute (config/delectus-content-bucket) $thingsid "name"))
;;; (time (get-object-attribute (config/delectus-content-bucket) $thingsid "NOPE!"))

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

;;; (def $thingsid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (get-list $thingsid)
;;; (time (get-object-attribute (config/delectus-content-bucket) $thingsid "test-attribute"))
;;; (update-object-attribute! (config/delectus-content-bucket) $thingsid "test-attribute" true)


;;; set the attribute, adding it to the object if it's not present
(defn upsert-object-attribute! [bucket objectid attribute-name value]
  (with-couchbase-exceptions-rethrown
   (let [mutator (.upsert (.mutateIn bucket objectid) attribute-name value)]
    (.execute mutator)
    value)))

;;; (def $thingsid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (get-list $thingsid)
;;; (time (get-object-attribute (config/delectus-content-bucket) $thingsid "test-attribute"))
;;; (time (upsert-object-attribute! (config/delectus-content-bucket) $thingsid "test-attribute" true))

;;; ---------------------------------------------------------------------

;;; Users
;;; ---------------------------------------------------------------------

(defn user-exists? [userid]
  (and (id-exists? (config/delectus-users-bucket) userid)
       (= +user-type+
          (get-object-attribute (config/delectus-users-bucket)
                                userid +type-attribute+))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (user-exists? $mikelid)
;;; (user-exists? "NOPE!")

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

;;; (email->user "mikel@evins.net")
;;; (email->user "greer@evins.net")
;;; (email->user "nobody@nowhere.net")


(defn email->userid [email]
  (let [found (find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (.get (first found) +id-attribute+))))

;;; (email->userid "mikel@evins.net")
;;; (email->userid "nobody@evins.net")

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
                    (json-object-type? obj +list-type+)
                    obj))))
      nil))

;;; (def $thingsid "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list $thingsid)
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-list $mikelid)
;;; (def $nopeid nil)
;;; (get-list $nopeid)
