(ns delectus-api-server.api-test
  (:require [clojure.pprint :as pp]
            [clojure.test :refer :all]
            [delectus-api-server.api :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.constants :refer :all]
            [delectus-api-server.couchio :as couchio]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.model :as model])
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.subdoc SubdocOptionsBuilder)))

;;; ---------------------------------------------------------------------
;;; User tests
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------

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

(deftest collection-with-id-test
  (testing "collection-with-id"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         collection (collection-named (model/email->userid email) "Default Collection")
         collection-id (.get collection +id-attribute+)
         found-collection (collection-with-id user-id collection-id)]
      (is (and (not (nil? found-collection))
               (instance? JsonObject found-collection)
               (couchio/json-object-type? found-collection +collection-type+))
          (pp/cl-format nil
                        "found-collection should be a collection object.~%~
  email = ~S~%~
  user-id = ~S~%~
  found-collection = ~S"
                        email user-id found-collection)))))


(deftest collection-name-test
  (testing "collection-name"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         collection (collection-named (model/email->userid email) "Default Collection")
         found-name (.get collection +name-attribute+)]
      (is (string? found-name)
          (pp/cl-format nil
                        "found-name should be a string.~%~
  email = ~S~%~
  user-id = ~S~%~
  found-name = ~S"
                        email user-id found-name)))))


(deftest collection-named-test
  (testing "collection-named"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name "Default Collection"
         collection (collection-named (model/email->userid email) test-name)
         found-name (if collection
                      (.get collection +name-attribute+)
                      nil)]
      (is (= found-name test-name)
          (pp/cl-format nil
                        "found-name should be a string equal to ~S.~%~
  email = ~S~%~
  user-id = ~S~%~
  found-name = ~S"
                        test-name email user-id found-name)))))

(deftest rename-collection-test
  (testing "rename-collection"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name1 (str "TEST-COLLECTION-1-" (makeid))
         test-name2 (str "TEST-COLLECTION-2-" (makeid))
         test-id (makeid)
         collection-id (new-collection :id test-id :name test-name1 :owner-id user-id)]
      (rename-collection user-id collection-id test-name2)
      (let [collection (couchio/get-collection collection-id)
            found-name (.get collection +name-attribute+)]
        (is (and (string? found-name)
                 (= found-name test-name2))
            (pp/cl-format nil
                          "found-name should be a string equal to ~S.~%~
  email = ~S~%~
  user-id = ~S~%~
  found-name = ~S"
                          test-name2 email user-id found-name))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-with-id $mikelid "1469fbd0-7d7d-41b2-8e5c-6db466129bcc")


(deftest new-collection-test
  (testing "new-collection"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name (str "TEST-COLLECTION-" (makeid))
         test-id (makeid)
         collection-id (new-collection :id test-id :name test-name :owner-id user-id)
         collection (couchio/get-collection collection-id)]
      (is (and (not (nil? collection))
               (instance? JsonObject collection)
               (couchio/json-object-type? collection +collection-type+))
          (pp/cl-format nil
                        "collection should be a collection object.~%~
  email = ~S~%~
  user-id = ~S~%~
  collection = ~S"
                        email user-id collection)))))

(deftest mark-collection-deleted-test
  (testing "mark-collection-deleted and collection-deleted?"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-id (makeid)
         test-name (str "TEST-COLLECTION-" test-id)
         collection-id (new-collection :id test-id :name test-name :owner-id user-id)]
      (mark-collection-deleted user-id test-id true)
      (let [collection (couchio/get-collection collection-id)]
        (is (and (not (nil? collection))
                 (instance? JsonObject collection)
                 (collection-deleted? user-id collection-id))
            "test collection should be deleted, but is not"))
      (mark-collection-deleted user-id test-id false)
      (let [collection (couchio/get-collection collection-id)]
        (is (and (not (nil? collection))
                 (instance? JsonObject collection)
                 (not (collection-deleted? user-id collection-id)))
            "test collection should not be deleted, but is")))))

(deftest collection-lists-test
  (testing "collection-lists, collection-add-list, collection-remove-list, new-list"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-id (makeid)
         test-collection-id (makeid)
         test-list-id (makeid)
         test-collection-name (str "TEST-COLLECTION-" test-id)
         test-list-name (str "TEST-LIST-" test-id)
         found-collection-if (new-collection :id test-collection-id :name test-collection-name :owner-id user-id)
         found-list-id (new-list :id test-list-id :name test-list-name :owner-id user-id)
         test-collection (couchio/get-collection test-collection-id)
         test-list (couchio/get-list test-list-id)]
      (is (and (not (nil? test-collection))
               (instance? JsonObject test-collection)
               (couchio/json-object-type? test-collection +collection-type+))
          "test collection not found")
      (is (and (not (nil? test-list))
               (instance? JsonObject test-list)
               (couchio/json-object-type? test-list +list-type+))
          "test list not found")
      (let [collection-lists (collection-lists user-id test-collection-id)]
        (is (empty? collection-lists) "test collection's lists should be empty before adding a list"))
      (collection-add-list user-id test-collection-id test-list-id)
      (let [collection-lists (collection-lists user-id test-collection-id)]
        (is (not (empty? collection-lists)) "test collection's lists shouldn't be empty after adding a list"))
      (collection-remove-list user-id test-collection-id test-list-id)
      (let [collection-lists (collection-lists user-id test-collection-id)]
        (is (empty? collection-lists) "test collection's lists should be empty after removing a list")))))

;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

(deftest new-list-test
  (testing "new-list"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name (str "TEST-LIST-" (makeid))
         test-id (makeid)
         list-id (new-list :id test-id :name test-name :owner-id user-id)
         the-list (couchio/get-list list-id)]
      (is (and (not (nil? the-list))
               (instance? JsonObject the-list)
               (couchio/json-object-type? the-list +list-type+))
          (pp/cl-format nil
                        "collection should be a List object.~%~
  email = ~S~%~
  user-id = ~S~%~
  list = ~S"
                        email user-id the-list)))))

