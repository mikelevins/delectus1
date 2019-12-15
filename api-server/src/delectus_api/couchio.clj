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
;;; error handling
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
;;; JsonDocument
;;; ---------------------------------------------------------------------

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; (make-json-document "foo_document" {"name" "Fred" "age" 35})
;;; (make-json-document "bar_document" {"name" "Fred" "age" 35 "things" {}})

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

;;; =====================================================================
;;; Fetching and storing documents by id
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

