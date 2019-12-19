(ns delectus-api.api-test
  (:require [clojure.pprint :as pp]
            [clojure.test :refer :all]
            [delectus-api.api :as api]
            [delectus-api.configuration :as config]
            [delectus-api.constants :refer :all]
            [delectus-api.couchio :as couchio]
            [delectus-api.identifiers :refer [makeid]]
            [delectus-api.model :as model])
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

(def +test-user1-id+ (:delectus-test-user1-id (config/delectus-configuration)))
(def +test-user1-email+ (:delectus-test-user1-email (config/delectus-configuration)))
(def +test-user1-password+ (:delectus-test-user1-password (config/delectus-configuration)))

(def +test-user2-id+ (:delectus-test-user2-id (config/delectus-configuration)))
(def +test-user2-email+ (:delectus-test-user2-email (config/delectus-configuration)))
(def +test-user2-password+ (:delectus-test-user2-password (config/delectus-configuration)))

(def +test-collection-id1+ "915ffccf-8261-473e-a82e-2b57404cb3b5")
(def +test-collection-name1+ "Test Collection 1")
(def +test-collection-id2+ "76856575-c8ba-42ed-af24-2c4f1589caf6")
(def +test-collection-name2+ "Test Collection 2")

;;; ---------------------------------------------------------------------
;;; setup and teardown
;;; ---------------------------------------------------------------------

(defn setup-test-data []
  (println "setting up test data...")
  (let [test-collection-1 (model/make-collection-document :name +test-collection-name1+
                                                          :owner-id +test-user1-id+
                                                          :id +test-collection-id1+)
        test-collection-2 (model/make-collection-document :name +test-collection-name2+
                                                          :owner-id +test-user1-id+
                                                          :id +test-collection-id2+)]
    (model/assert-collection! test-collection-1)
    (model/assert-collection! test-collection-2))
  ;;; wait after setup to make sure DB's API returns consistent results
  (Thread/sleep 1000))

;;; (setup-test-data)
;;; (api/collections (:delectus-test-user1-id (config/delectus-configuration)))
;;; (lists (model/email->userid (:delectus-test-user (config/delectus-configuration))))

(defn teardown-test-data []
  (println "deleting test data...")
  ;;; wait before teardown to make sure DB's API returns consistent results
  (Thread/sleep 1000)
  (let [test-documents (couchio/find-objects (config/delectus-content-bucket) []
                                             {"owner-id" +test-user1-id+})]
    (doseq [doc test-documents]
      (couchio/remove-document! (config/delectus-content-bucket)
                                (.get doc +id-attribute+))))
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

(deftest authenticate-test
  (testing "/api/user/authenticate"
    (let [found-user (api/authenticate +test-user1-id+ +test-user1-password+)]
      (is (and found-user
               (= +user-type+ (.get found-user +type-attribute+)))
          "found-user should be a user object"))))

(deftest login-test
  (testing "/api/user/login"
    (let [found-user (api/login +test-user1-email+ +test-user1-password+)]
      (is (and found-user
               (= +user-type+ (.get found-user +type-attribute+)))
          "found-user should be a user object"))))

(deftest userid-test
  (testing "/api/user/userid"
    (let [found-id (api/userid +test-user1-email+)]
      (is found-id "found-id should be a user ID string")
      (is (= found-id +test-user1-id+)
          "found-id should equal to the standard test-user1 ID"))))

(deftest userdata-test
  (testing "/api/user/userdata"
    (let [data (api/userdata +test-user1-id+)]
      (is (= (get data +id-attribute+) +test-user1-id+)
          "userid should be the standard test-user ID string")
      (is (= (get data +email-attribute+) +test-user1-email+)
          "email should be the standard test-user email string"))))

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------

(deftest collections-test
  (testing "/api/collection/collections"
    (let [collections (api/collections +test-user1-id+)]
      (is (= 2 (count collections))
          (str "there should be 2 test collections but found " (count collections)))
      (is (every? #(= +collection-type+ (.get % +type-attribute+)) collections)
          "all found objects should be collections"))))

(deftest collection-with-id-test
  (testing "/api/collection/collection_with_id"
    (let [collection (api/collection-with-id +test-user1-id+ +test-collection-id1+)]
      (is (= (get collection +owner-id-attribute+)
             +test-user1-id+)
          "the collection's owner-id should be the standard test-user1 ID")
      (is (= (get collection +name-attribute+)
             +test-collection-name1+)
          "the collection's owner-id should be the standard test-user1 ID"))))

;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

