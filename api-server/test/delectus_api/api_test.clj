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

;;; ---------------------------------------------------------------------
;;; setup and teardown
;;; ---------------------------------------------------------------------

(defn setup-test-data []
  (println "setting up test data...")

  ;;; wait after setup to make sure DB's API returns consistent results
  (Thread/sleep 1000))

;;; (setup-test-data)
;;; (collections (model/email->userid (:delectus-test-user (config/delectus-configuration))))
;;; (lists (model/email->userid (:delectus-test-user (config/delectus-configuration))))

(defn teardown-test-data []
  (println "deleting test data...")
  ;;; wait before teardown to make sure DB's API returns consistent results
  (Thread/sleep 1000)
  
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
    (let [email (:delectus-test-user1-email (config/delectus-configuration))
          password (:delectus-test-user1-password (config/delectus-configuration))
          found-user (api/login email password)]
      (is (and found-user
               (= +user-type+ (.get found-user +type-attribute+)))
          "found-user should be a user object"))))

(deftest userid-test
  (testing "/api/user/userid"
    (let [email (:delectus-test-user1-email (config/delectus-configuration))
          found-id (api/userid email)]
      (is found-id "found-id should be a user ID string")
      (is (= found-id (:delectus-test-user1-id (config/delectus-configuration)))
          "found-id should equal to the standard test user ID"))))

(deftest userdata-test
  (testing "/api/user/userdata"
    (let [data (api/userdata (:delectus-test-user1-id (config/delectus-configuration)))]
      (is (= (get data +id-attribute+) (:delectus-test-user1-id (config/delectus-configuration)))
          "userid should be the standard test-user ID string")
      (is (= (get data +email-attribute+) (:delectus-test-user1-email (config/delectus-configuration)))
          "email should be the standard test-user email string"))))

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

