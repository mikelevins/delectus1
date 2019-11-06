(ns delectus-api-server.couchio
  (:require
   [delectus-api-server.configuration :as config])
  (:import
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)))

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
;;; (make-object-selector (config/delectus-users-bucket) ["id" "type"] {"type" "delectus_list" "id" "FOO!"})

(defn find-objects [bucket keys matching]
  (let [selector (make-object-selector bucket keys matching)
        bucket-name (.name bucket)
        results (.query bucket (N1qlQuery/simple selector))]
    (if (empty? keys)
      ;; SELECT * returns each result in a map like {bucket-name found-object}
      (map #(.get (.value %) bucket-name) results)
      (map #(.value %) results))))

;;; (def $objs (find-objects (config/delectus-content-bucket) [] {"type" "delectus_list"}))
;;; (def $objs (find-objects (config/delectus-content-bucket) ["name" "id"] {"type" "delectus_list"}))

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
