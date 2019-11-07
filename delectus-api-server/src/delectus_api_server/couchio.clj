(ns delectus-api-server.couchio
  (:require
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :as constants])
  (:import
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)))


;;; ---------------------------------------------------------------------
;;; document and object helpers
;;; ---------------------------------------------------------------------

(defn json-object-type? [obj type-string]
  (and (instance? JsonObject obj)
       (= (.get obj "type")
          type-string)))


(defn json-object-owner? [obj ownerid]
  (and (instance? JsonObject obj)
       (= (.get obj "owner-id")
          ownerid)))

;;; ---------------------------------------------------------------------
;;; simple fetch and store by id
;;; ---------------------------------------------------------------------

(defn get-document [bucket docid]
  (.get bucket docid))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $mikelid (delectus-api-server.api/email->userid "mikel@evins.net"))
;;; (def $docid (.get (delectus-api-server.api/find-collection-by-name $mikelid  "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))

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
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-collection $mikelid)
;;; (def $nopeid nil)
;;; (get-collection $nopeid)

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
      ;; SELECT * returns each result in a map like {bucket-name found-object}
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
