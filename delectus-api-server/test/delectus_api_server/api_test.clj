(ns delectus-api-server.api-test
  (:require [clojure.pprint :as pp]
            [clojure.test :refer :all]
            [delectus-api-server.api :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.constants :refer :all]
            [delectus-api-server.couchio :as couchio])
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))

(deftest login-test
  (testing "login"
    (let [username (:delectus-test-user (config/delectus-configuration))
          password (:delectus-test-user-password (config/delectus-configuration))
          found-user (login username password)]
      (is found-user "found-user should be a user object"))))

(deftest userid-test
  (testing "userid"
    (let [email (:delectus-test-user (config/delectus-configuration))
          found-id (userid email)]
      (is found-id "found-id should be a user ID string"))))


(deftest collections-test
  (testing "collections"
    (let [email (:delectus-test-user (config/delectus-configuration))
          user-id (userid email)
          found-collections (collections user-id)]
      (is (and found-collections
               (not (empty? found-collections))
               (every? #(and (instance? JsonObject %)
                             (couchio/json-object-type? % +collection-type+))
                       found-collections))
          (pp/cl-format nil
                        "found-collections should be a list of collection objects.~%~
  email = ~S~%~
  user-id = ~S~%~
  found-collections = ~S"
                        email user-id found-collections)))))

