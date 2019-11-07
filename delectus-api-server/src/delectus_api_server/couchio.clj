(ns delectus-api-server.couchio
  (:require
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :as constants]
   [delectus-api-server.errors :as errors])
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))


;;; ---------------------------------------------------------------------
;;; experiments
;;; ---------------------------------------------------------------------

;;; (def $bucket (config/scratch-bucket))

;;; (def $fruitsid "fruit_list")
;;; (def $fruits (CouchbaseArrayList. $fruitsid $bucket ["Apple" "Banana" "Cherry" "Date" "Eggplant"]))
;;; (class $fruits)
;;; (time (.get $fruits 3))
;;; (time (.set $fruits 3 "Durian"))
;;; (def $fruits2 (CouchbaseArrayList. $fruitsid $bucket))

;;; (def $lookupin (.lookupIn $bucket $fruitsid))
;;; (def $optionsbuilder (SubdocOptionsBuilder.))
;;; (.get $lookupin "$document.id" $optionsbuilder)
;;; (.execute (.get $lookupin "$document.id" $optionsbuilder))

;;; (def $fredid "fred_map")
;;; (def $fred (CouchbaseMap. $fredid $bucket {"name" "Fred" "age" 35 "friends" (JsonArray/from ["Barney" "Betty"])}))
;;; (class $fred)
;;; (def $fred2 (CouchbaseMap. $fredid $bucket))


;;; ---------------------------------------------------------------------
;;; document and object helpers
;;; ---------------------------------------------------------------------

;;; accessors
;;; ---------------

(defn json-object-type [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  (.get obj constants/+json-object-type-attribute+))

(defn json-object-owner-id [obj]
  (errors/error-if-not (instance? JsonObject obj) "Not JSON object" {:object obj})
  (.get obj constants/+json-object-owner-id-attribute+))

(defn json-object-items [obj]
  (errors/error-if-not (itemizing-json-object? obj) "Not an itemizing JSON object" {:object obj})
  (.get obj constants/+items-attribute+))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (json-object-items (get-collection $defaultid))

(defn json-object-max-item-index [obj]
  (errors/error-if-not (itemizing-json-object? obj) "Not an itemizing JSON object" {:object obj})
  (let [items (json-object-items obj)
        indexes (into [] (.getNames items))]
    (if (empty? indexes) nil
        (str (apply max (map edn/read-string (into [] indexes)))))))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (json-object-max-item-index (get-collection $defaultid))

(defn json-object-next-item-index [obj]
  (errors/error-if-not (itemizing-json-object? obj) "Not an itemizing JSON object" {:object obj})
  (let [items (json-object-items obj)
        indexes (into [] (.getNames items))]
    (if (empty? indexes) "0"
        (str (+ 1 (apply max (map edn/read-string (into [] indexes))))))))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (json-object-next-item-index (get-collection $defaultid))

;;; predicates
;;; ---------------

(defn json-object-type? [obj type-string]
  (= type-string (json-object-type obj)))

(defn json-object-owner? [obj ownerid]
  (= ownerid (json-object-owner-id obj)))

(defn itemizing-json-object? [obj]
  (and (instance? JsonObject obj)
       (or (json-object-type? obj constants/+delectus-collection-document-type+)
           (json-object-type? obj constants/+delectus-list-document-type+))))

;;; (def $fred (make-json-object {"name" "Fred" "age" 35}))
;;; (itemizing-json-object? $fred)
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (itemizing-json-object? (get-collection $defaultid))

;;; constructors
;;; ---------------

(defn make-json-object [object-map]
  (JsonObject/from object-map))

;;; (make-json-object {"name" "Fred" "age" 35})
;;; (make-json-object {"name" "Fred" "age" 35 "things" {}})

(defn make-json-document [id object-map]
  (JsonDocument/create id (JsonObject/from object-map)))

;;; (make-json-document "foo_document" {"name" "Fred" "age" 35})
;;; (make-json-document "bar_document" {"name" "Fred" "age" 35 "things" {}})

(defn make-collection-document [id name ownerid]
  (let [obj-map {constants/+type-attribute+ constants/+delectus-collection-document-type+
                 constants/+id-attribute+ id
                 constants/+name-attribute+ name
                 constants/+owner-id-attribute+ ownerid
                 constants/+items-attribute+ {}}]
    (make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-collection-document "foo_collection" "Random stuff" $mikelid)q

;;; ---------------------------------------------------------------------
;;; simple fetch and store by id
;;; ---------------------------------------------------------------------

;;; generic JsonDocuments
;;; ----------------------

(defn get-document [bucket docid]
  (.get bucket docid))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (def $docid (.get (delectus-api-server.api/find-collection-by-name $mikelid  "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))

;;; User objects
;;; ----------------------

(defn get-user [userid]
  (or (and userid
           (let [candidate (get-document (config/delectus-users-bucket) userid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj constants/+delectus-user-document-type+)
                    obj))))
      nil))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user $mikelid)
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-user $defaultid)
;;; (def $nopeid nil)
;;; (get-user $nopeid)

;;; Collection objects
;;; ----------------------

(defn get-collection [collectionid]
  (or (and collectionid
           (let [candidate (get-document (config/delectus-content-bucket) collectionid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj constants/+delectus-collection-document-type+)
                    obj))))
      nil))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection $defaultid)
;;; (def $bucket (config/delectus-content-bucket))
;;; (.content (.execute (.get (.lookupIn $bucket $defaultid) (into-array ["items"]))) 0)
;;; (.content (.execute (.get (.lookupIn $bucket $defaultid) (into-array ["NOPE"]))) 0)

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-collection $mikelid)
;;; (def $nopeid nil)
;;; (get-collection $nopeid)

;;; List objects
;;; ----------------------

(defn get-list [listid]
  (or (and listid
           (let [candidate (get-document (config/delectus-content-bucket) listid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (json-object-type? obj constants/+delectus-list-document-type+)
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
;;; (make-object-selector (config/delectus-users-bucket) ["id" "type"] {"type" constants/+delectus-list-document-type+ "id" "FOO!"})

(defn find-objects [bucket keys matching]
  (let [selector (make-object-selector bucket keys matching)
        bucket-name (.name bucket)
        results (.query bucket (N1qlQuery/simple selector))]
    (if (empty? keys)
      ;; SELECT * returns each result wrapped in a map like this: {bucket-name found-object}
      (map #(.get (.value %) bucket-name) results)
      (map #(.value %) results))))

;;; (def $objs (find-objects (config/delectus-content-bucket) [] {"type" constants/+delectus-list-document-type+}))
;;; (def $objs (find-objects (config/delectus-content-bucket) ["name"] {"type" constants/+delectus-list-document-type+}))

;;; ---------------------------------------------------------------------
;;; JsonDocument helpers
;;; ---------------------------------------------------------------------

(defn put-key-if-changed [json-obj key new-value]
  (let [has-key? (.containsKey json-obj key)
        value-changed? (if has-key?
                         (not (= new-value (.get json-obj key)))
                         true)]
    (if value-changed?
      (JsonObject/from (merge (into {} (.toMap json-obj))
                              {key new-value}))
      json-obj)))

;;; (def $obj1 (JsonObject/from {"name" "Fred"}))
;;; (def $obj2 (put-key-if-changed $obj1 "name" "Fred"))
;;; (def $obj3 (put-key-if-changed $obj1 "name" "Barney"))

(defn find-json-object-key-for-value [obj val]
  (let [props (into [] (.getNames obj))]
    (some #(and (= val (.get obj %))
                %)
          props)))

;;; (def $obj1 (JsonObject/from {"name" "Fred" "age" 35 "color" "orange"}))
;;; (find-json-object-key-for-value $obj1 "orange")

;;; ---------------------------------------------------------------------
;;; couchio errors
;;; ---------------------------------------------------------------------

(defn error-if-collection-id-exists [id]
  (let [found (get-document (config/delectus-content-bucket) id)]
    (if found
      (throw (ex-info "An object with this ID already exists"
                      {:id id
                       :bucket "delectus-content-bucket"
                       :error-signaled-by 'error-if-collection-id-exists})))))
