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

(def +test-data-prefix+ "DELECTUS-TEST-DATA::")

;;; make-test-id []
;;; ---------------------------------------------------------------------
;;; use make-test-id instead of makeid for IDs used in tests; make-test-id
;;; constructs IDs the we can readily recognize as ephemeral test data
;;; to be deleted from the database when tests are finished running
(defn make-test-id []
  (str +test-data-prefix+ (makeid)))

(defn make-test-name [name]
  (str name "::" (makeid)))

(def +test-user-id+ (:delectus-test-user-id (config/delectus-configuration)))
(def +test-user-email+ (:delectus-test-user-email (config/delectus-configuration)))
(def +test-user-password+ (:delectus-test-user-password (config/delectus-configuration)))

;;; ---------------------------------------------------------------------
;;; keeping track of test data
;;; ---------------------------------------------------------------------

(def  +test-data-map+ (atom {}))

(defn get-test-data [key]
  (get @+test-data-map+ key nil))

;;; (get-test-data :foo)

(defn reset-test-data! []
  (swap! +test-data-map+ (constantly {})))

;;; (assert-test-data! :foo "Bar")
;;; (reset-test-data!)
;;; (get-test-data :foo)

(defn assert-test-data! [key val]
  (swap! +test-data-map+ assoc key val))

;;; (assert-test-data! :foo "Bar")
;;; (get-test-data :foo)

;;; ---------------------------------------------------------------------
;;; setup and teardown
;;; ---------------------------------------------------------------------

(defn setup-test-data []
  (println "setting up test data...")

  ;;; wait after setup to make sure DB's API returns consistent results
  (Thread/sleep 2000))

;;; (setup-test-data)
;;; (collections (model/email->userid (:delectus-test-user (config/delectus-configuration))))
;;; (lists (model/email->userid (:delectus-test-user (config/delectus-configuration))))

(defn teardown-test-data []
  (println "deleting test data...")
  ;;; wait before teardown to make sure DB's API returns consistent results
  (Thread/sleep 2000)

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
    (let [userid (:delectus-test-user-id (config/delectus-configuration))
          password (:delectus-test-user-password (config/delectus-configuration))
          found-user (api/authenticate userid password)]
      (is found-user "found-user should be a user object"))))

;;; (def $userid (:delectus-test-user-id (config/delectus-configuration)))
;;; (def $password (:delectus-test-user-password (config/delectus-configuration)))
;;; (authenticate $userid $password)

(deftest login-test
  (testing "/api/user/login"
    (let [email (:delectus-test-user-email (config/delectus-configuration))
          password (:delectus-test-user-password (config/delectus-configuration))
          found-user (api/login email password)]
      (is found-user "found-user should be a user object"))))

(deftest userid-test
  (testing "/api/user/userid"
    (let [email (:delectus-test-user-email (config/delectus-configuration))
          found-id (api/userid email)]
      (is found-id "found-id should be a user ID string")
      (is (= found-id (:delectus-test-user-id (config/delectus-configuration)))
          "found-id should equal to the standard test user ID"))))

(deftest userdata-test
  (testing "/api/user/userdata"
    (let [data (api/userdata (:delectus-test-user-id (config/delectus-configuration)))]
      (is (= (:userid data) (:delectus-test-user-id (config/delectus-configuration)))
          "userid should be the standard test-user ID string")
      (is (= (:email data) (:delectus-test-user-email (config/delectus-configuration)))
          "email should be the standard test-user email string"))))

;;; (def $testid (couchio/email->userid (:delectus-test-user-email (config/delectus-configuration))))

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

