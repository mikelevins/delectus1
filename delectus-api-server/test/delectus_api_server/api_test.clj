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
;;; testing helpers
;;; ---------------------------------------------------------------------
;;; functions and data for marking and managing test data
;;; (that is, data created by Delectus unit tests)

(def +test-data-prefix+ "DELECTUS-TEST-DATA::")

(defn make-test-id []
  (str +test-data-prefix+ (makeid)))

(defn make-test-name [name]
  (str name "::" (makeid)))

(def +stable-test-collection-id+ (str +test-data-prefix+ "Collection-0::" "029ef6f7-5170-4671-89b7-386ef1156c2d"))
(def +stable-test-collection-name+ (str "Collection-0::" "7e1c04c3-4d05-41b3-81d4-a67d64c17092"))
(def +stable-test-collection-alternate-name+ (str "Collection-0::" "2677e446-3f07-44bc-8be8-785af279c6e1"))
(def +stable-test-list-0-id+ (str +test-data-prefix+  "List-0::" "23d4dce0-93f2-4983-a59e-cff092f8a987"))
(def +stable-test-list-0-name+ (str  "List-0::" "f8047f56-bbc2-414b-a44b-86aefcc502a4"))
(def +stable-test-list-1-id+ (str +test-data-prefix+  "List-1::" "905c6ab2-06a2-43dc-bf98-6fa9996bd64d"))
(def +stable-test-list-1-name+ (str  "List-1::" "0e803fce-38be-4138-8456-e78b98366e5d"))
(def +stable-test-list-2-id+ (str +test-data-prefix+  "List-2::" "18c1dbdc-191c-4ac3-9994-abaac99d5522"))
(def +stable-test-list-2-name+ (str  "List-2::" "56cc71c9-ef89-426d-971f-0baed5e511c6"))

;;; finds objects whose IDs are prefixed with the +test-data-prefix+
(defn find-test-data [bucket]
  (let [bucket-name (.name bucket)
        selector (str "SELECT * from `" bucket-name "` "
                      "WHERE id LIKE \"" +test-data-prefix+ "%\"")
        results (.query bucket (N1qlQuery/simple selector))]
    (map #(.get (.value %) bucket-name) results)))

;;; (find-test-data (config/delectus-users-bucket))
;;; (find-test-data (config/delectus-content-bucket))

;;; deletes objects whose IDs are prefixed with the +test-data-prefix+
(defn delete-test-data [bucket]
  (doall (map #(.remove bucket (.get % +id-attribute+))
              (find-test-data bucket))))

;;; (delete-test-data (config/delectus-users-bucket))
;;; (delete-test-data (config/delectus-content-bucket))

;;; ---------------------------------------------------------------------
;;; setup and teardown
;;; ---------------------------------------------------------------------

(defn setup-test-data []
  (println "setting up test data...")
  (let [email (:delectus-test-user (config/delectus-configuration))
        user-id (model/email->userid email)]
    ;;; stable test data
    (new-collection :id +stable-test-collection-id+ :name +stable-test-collection-name+ :owner-id user-id)
    (new-list :id +stable-test-list-0-id+ :name +stable-test-list-0-name+ :owner-id user-id)
    (new-list :id +stable-test-list-1-id+ :name +stable-test-list-1-name+ :owner-id user-id)
    (new-list :id +stable-test-list-2-id+ :name +stable-test-list-2-name+ :owner-id user-id)
    ;;; randomly generated names and ids
    (new-collection :id (make-test-id) :name (make-test-name "Collection-1") :owner-id user-id)
    (new-collection :id (make-test-id) :name (make-test-name "Collection-2") :owner-id user-id))
  ;;; wait after setup to make sure DB's API returns consistent results
  (Thread/sleep 2000))

;;; (setup-test-data)
;;; (collections (model/email->userid (:delectus-test-user (config/delectus-configuration))))
;;; (lists (model/email->userid (:delectus-test-user (config/delectus-configuration))))

(defn teardown-test-data []
  (println "deleting test data...")
  ;;; wait before teardown to make sure DB's API returns consistent results
  (Thread/sleep 2000)
  (delete-test-data (config/delectus-users-bucket))
  (delete-test-data (config/delectus-content-bucket))
  (println "Finished."))

(defn test-fixture [f]
  (setup-test-data)
  (f)
  (teardown-test-data))

;; register as a one-time callback
(use-fixtures :once test-fixture)

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
         found-collection (collection-with-id user-id +stable-test-collection-id+)]
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
         found-collection (collection-with-id user-id +stable-test-collection-id+)
         found-name (.get found-collection +name-attribute+)]
      (is (and (string? found-name)
               (= found-name +stable-test-collection-name+))
          (pp/cl-format nil "found-name should be ~S but found ~S"
                        +stable-test-collection-name+
                        found-name)))))


(deftest collection-named-test
  (testing "collection-named"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         found-collection (collection-named user-id +stable-test-collection-name+)
         found-id (.get found-collection +id-attribute+)]
      (is (and (string? found-id)
               (= found-id +stable-test-collection-id+))
          (pp/cl-format nil "found-id should be ~S" +stable-test-collection-id+)))))


(deftest rename-collection-test
  (testing "rename-collection"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         collection (couchio/get-collection +stable-test-collection-id+)]
      ;; rename the test collection
      (rename-collection user-id +stable-test-collection-id+ +stable-test-collection-alternate-name+)
      (let [collection (couchio/get-collection +stable-test-collection-id+)
            found-name (.get collection +name-attribute+)]
        (is (and (string? found-name)
                 (= found-name +stable-test-collection-alternate-name+))
            (pp/cl-format nil "found-name should be ~S, but found ~S"
                          +stable-test-collection-alternate-name+
                          found-name)))
      ;; change the name back
      (rename-collection user-id +stable-test-collection-id+ +stable-test-collection-name+)
      (let [collection (couchio/get-collection +stable-test-collection-id+)
            found-name (.get collection +name-attribute+)]
        (is (and (string? found-name)
                 (= found-name +stable-test-collection-name+))
            (pp/cl-format nil "found-name should be ~S, but found ~S"
                          +stable-test-collection-alternate-name+
                          found-name))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-with-id $mikelid "1469fbd0-7d7d-41b2-8e5c-6db466129bcc")

(deftest new-collection-test
  (testing "new-collection"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name (str "NEW-TEST-COLLECTION-" (makeid))
         test-id (make-test-id)
         collection-id (new-collection :id test-id :name test-name :owner-id user-id)
         collection (couchio/get-collection collection-id)]
      (is (and (not (nil? collection))
               (instance? JsonObject collection)
               (couchio/json-object-type? collection +collection-type+))
          (pp/cl-format nil
                        "object ~S should be a collection but found ~S"
                        collection-id collection)))))

(deftest mark-collection-deleted-test
  (testing "mark-collection-deleted and collection-deleted?"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         collection (couchio/get-collection +stable-test-collection-id+)]
      (mark-collection-deleted user-id +stable-test-collection-id+ true)
      (is (and (not (nil? collection))
               (instance? JsonObject collection)
               (collection-deleted? user-id +stable-test-collection-id+))
          "test collection should be deleted, but is not")
      (mark-collection-deleted user-id +stable-test-collection-id+ false)
      (is (and (not (nil? collection))
               (instance? JsonObject collection)
               (not (collection-deleted? user-id +stable-test-collection-id+)))
          "test collection should not be deleted, but is"))))

(deftest collection-lists-test
  (testing "collection-lists, collection-add-list, collection-remove-list"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-collection (couchio/get-collection +stable-test-collection-id+)
         test-list-0 (couchio/get-collection +stable-test-list-0-id+)
         test-list-1 (couchio/get-collection +stable-test-list-1-id+)
         test-list-2 (couchio/get-collection +stable-test-list-2-id+)]
      (is (and (not (nil? test-collection))
               (instance? JsonObject test-collection)
               (couchio/json-object-type? test-collection +collection-type+))
          "test collection not found")
      (is (and (not (nil? test-list-0))
               (instance? JsonObject test-list-0)
               (couchio/json-object-type? test-list-0 +list-type+))
          "test list 0 not found")
      (is (and (not (nil? test-list-1))
               (instance? JsonObject test-list-1)
               (couchio/json-object-type? test-list-1 +list-type+))
          "test list 1 not found")
      (is (and (not (nil? test-list-2))
               (instance? JsonObject test-list-2)
               (couchio/json-object-type? test-list-2 +list-type+))
          "test list 2 not found")
      (let [collection-lists (collection-lists user-id +stable-test-collection-id+)]
        (is (empty? collection-lists) "test collection's lists should be empty before adding a list"))
      ;; add lists
      (collection-add-list user-id +stable-test-collection-id+ +stable-test-list-0-id+)
      (collection-add-list user-id +stable-test-collection-id+ +stable-test-list-1-id+)
      (collection-add-list user-id +stable-test-collection-id+ +stable-test-list-2-id+)
      ;; check the collection's lists
      (let [collection-lists (collection-lists user-id +stable-test-collection-id+)]
        (is (and (not (empty? collection-lists))
                 (= 3 (count collection-lists)))
            (pp/cl-format nil "test collection's lists should contain 3 members, not ~S" (count collection-lists))))
      ;; remove lists
      (collection-remove-list user-id +stable-test-collection-id+ +stable-test-list-0-id+)
      (collection-remove-list user-id +stable-test-collection-id+ +stable-test-list-1-id+)
      (collection-remove-list user-id +stable-test-collection-id+ +stable-test-list-2-id+)
      (let [collection-lists (collection-lists user-id +stable-test-collection-id+)]
        (is (empty? collection-lists)
            (pp/cl-format nil "test collection's lists should be empty after removing lists but found ~S"
                          collection-lists))))))

;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

(deftest new-list-test
  (testing "new-list"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         test-name (str "TEST-LIST-" (makeid))
         test-id (make-test-id)
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

