(ns delectus-api.api-test
  (:require [clojure.pprint :as pp]
            [clojure.test :refer :all]
            [delectus-api.api :refer :all]
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

(def +stable-test-collection-id+ (str +test-data-prefix+ "Collection-0::" "029ef6f7-5170-4671-89b7-386ef1156c2d"))
(def +stable-test-collection-name+ (str "Collection-0::" "7e1c04c3-4d05-41b3-81d4-a67d64c17092"))
(def +stable-test-collection-alternate-name+ (str "Collection-0::" "Alternate-7e1c04c3-4d05-41b3-81d4-a67d64c17092"))
(def +stable-test-list-0-id+ (str +test-data-prefix+  "List-0::" "23d4dce0-93f2-4983-a59e-cff092f8a987"))
(def +stable-test-list-0-name+ (str  "List-0::" "f8047f56-bbc2-414b-a44b-86aefcc502a4"))
(def +stable-test-list-1-id+ (str +test-data-prefix+  "List-1::" "905c6ab2-06a2-43dc-bf98-6fa9996bd64d"))
(def +stable-test-list-1-name+ (str  "List-1::" "0e803fce-38be-4138-8456-e78b98366e5d"))
(def +stable-test-list-1-alternate-name+ (str  "List-1::" "Alternate-0e803fce-38be-4138-8456-e78b98366e5d"))
(def +stable-test-list-2-id+ (str +test-data-prefix+  "List-2::" "18c1dbdc-191c-4ac3-9994-abaac99d5522"))
(def +stable-test-list-2-name+ (str  "List-2::" "56cc71c9-ef89-426d-971f-0baed5e511c6"))
(def +stable-test-column-name-a+ "Column A")
(def +stable-test-column-name-b+ "Column B")

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
    (let [email (:delectus-test-user (config/delectus-configuration))
          password (:delectus-test-user-password (config/delectus-configuration))
          found-user (login email password)]
      (is found-user "found-user should be a user object"))))

(deftest userid-test
  (testing "userid"
    (let [email (:delectus-test-user (config/delectus-configuration))
          found-id (couchio/email->userid email)]
      (is found-id "found-id should be a user ID string"))))

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

