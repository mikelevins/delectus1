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


;;; ---------------------------------------------------------------------
;;; how to create and access Couchbase data structures
;;; ---------------------------------------------------------------------
;;; NOTE: creating a Couchbase data structure creates the object in the
;;;       store if it does not already exist. It is therefore
;;;       necessary to check for the existence of an id in the store
;;;       using id-exists?  before calling the constructors, to avoid
;;;       littering the bucket with gratuitous objects
;;;
;;; (def $bucket (config/scratch-bucket))
;;;
;;; CouchbaseArrayList
;;;
;;; (def $fruitsid "fruit_list")
;;; (def $fruits (CouchbaseArrayList. $fruitsid $bucket ["Apple" "Banana" "Cherry" "Date" "Eggplant"]))
;;; (class $fruits)
;;; (time (.get $fruits 3))
;;; (time (.set $fruits 3 "Durian"))
;;; (def $fruits2 (CouchbaseArrayList. $fruitsid $bucket))
;;;
;;; CouchbaseMap
;;;
;;; (def $fredid "fred_map")
;;; (def $fred (CouchbaseMap. $fredid $bucket {"name" "Fred" "age" 35 "friends" (JsonArray/from ["Barney" "Betty"])}))
;;; (class $fred)
;;; (def $fred2 (CouchbaseMap. $fredid $bucket))
;;;
;;; lookupIn
;;;
;;; (def $lookupin (.lookupIn $bucket $fruitsid))
;;; (def $optionsbuilder (SubdocOptionsBuilder.))
;;; (.get $lookupin "$document.id" $optionsbuilder)
;;; (.execute (.get $lookupin "$document.id" $optionsbuilder))


;;; ---------------------------------------------------------------------
;;; document and object helpers
;;; ---------------------------------------------------------------------

;;; accessors
;;; ---------------------------------------------------------------------

(defn json-object-type [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  (.get obj +type-attribute+))

(defn json-object-owner-id [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  (.get obj +owner-id-attribute+))

(defn json-object-items [obj]
  (.get obj +items-attribute+))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (json-object-items (get-collection $defaultid))

;;; predicates
;;; ---------------------------------------------------------------------

(defn json-object-type? [obj type-string]
  (= type-string (json-object-type obj)))

(defn json-object-owner? [obj ownerid]
  (= ownerid (json-object-owner-id obj)))

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

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; (make-json-document "foo_document" {"name" "Fred" "age" 35})
;;; (make-json-document "bar_document" {"name" "Fred" "age" 35 "things" {}})

(defn make-user-document [& {:keys [id email name password-hash enabled]
                             :or {id (makeid)
                                  email nil
                                  name nil
                                  password-hash nil
                                  enabled true}}]
  (errors/error-if-nil email "Missing email parameter" {:context "make-user-document"})
  (let [obj-map {+type-attribute+ +user-type+
                 +id-attribute+ id
                 +email-attribute+ email
                 +name-attribute+ name
                 +password-hash-attribute+ password-hash
                 +enabled-attribute+ enabled}]
    (make-json-document id obj-map)))

;;; (make-user-document :email "mikel@evis.net")

(defn make-collection-document [& {:keys [id name owner-id lists deleted]
                                   :or {id (makeid)
                                        name nil
                                        owner-id nil
                                        ;; lists is a set of list-ids
                                        ;; we represent that as a JSON object
                                        ;; the keys are the lists, the vals are ignored
                                        lists {}
                                        deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-collection-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-collection-document"})
  (let [obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ lists
                 +deleted-attribute+ deleted}]
    (make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-collection-document :name "Random stuff" :owner-id $mikelid)

(defn make-list-document [& {:keys [id name owner-id columns items deleted]
                             :or {id (makeid)
                                  name nil
                                  owner-id nil
                                  columns {}
                                  items []
                                  deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-list-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +list-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +columns-attribute+ columns
                 +items-attribute+ items
                 +deleted-attribute+ deleted}]
    (make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-list-document :name "Random stuff" :owner-id $mikelid)

;;; ---------------------------------------------------------------------
;;; fetch and store by id
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

;;; User objects
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

;;; Collection objects
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

;;; List objects
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

;;; ---------------------------------------------------------------------
;;; N1QL queries
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

;;; ---------------------------------------------------------------------
;;; JsonDocument helpers
;;; ---------------------------------------------------------------------

(defn put-key-if-changed [json-obj key new-value]
  (let [changed? (if (.containsKey json-obj key)
                   (not (= new-value (.get json-obj key)))
                   true)]
    (if changed?
      (JsonObject/from (merge (into {} (.toMap json-obj))
                              {key new-value}))
      json-obj)))

;;; (def $obj1 (JsonObject/from {"name" "Fred"}))
;;; (def $obj2 (put-key-if-changed $obj1 "name" "Fred"))
;;; (def $obj3 (put-key-if-changed $obj1 "name" "Barney"))

(defn find-json-object-key-for-value [obj val]
  (let [the-keys (into [] (.getNames obj))]
    (some (fn [key]
            (and (= val (.get obj key))
                 key))
          the-keys)))

;;; (def $obj1 (JsonObject/from {"name" "Fred" "age" 35 "color" "orange"}))
;;; (find-json-object-key-for-value $obj1 35)

;;; ---------------------------------------------------------------------
;;; couchio errors
;;; ---------------------------------------------------------------------

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

