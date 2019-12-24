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

(def +test-user-id-1+ (:delectus-test-user1-id (config/delectus-configuration)))
(def +test-user-email-1+ (:delectus-test-user1-email (config/delectus-configuration)))
(def +test-user-password-1+ (:delectus-test-user1-password (config/delectus-configuration)))

(def +test-user-id-2+ (:delectus-test-user2-id (config/delectus-configuration)))
(def +test-user-email-2+ (:delectus-test-user2-email (config/delectus-configuration)))
(def +test-user-password-2+ (:delectus-test-user2-password (config/delectus-configuration)))

(def +test-collection-id-1+ "915ffccf-8261-473e-a82e-2b57404cb3b5")
(def +test-collection-name-1+ "Test Collection 1")
(def +test-collection-alt-name-1+ "Test Collection (alternate name) 1")
(def +test-collection-id-2+ "76856575-c8ba-42ed-af24-2c4f1589caf6")
(def +test-collection-name-2+ "Test Collection 2")

(def +test-new-collection-name+ "New Collection for Testing")

(def +test-list-id-1+ "6957a4ba-75ae-4957-af82-529cd016341d")
(def +test-list-name-1+ "Test List 1")
(def +test-list-alt-name-1+ "Test List (alternate name) 1")

(def +test-list-id-2+ "e1cf3bf3-ce80-4fdc-92e6-3e7a33c14fd2")
(def +test-list-name-2+ "Test List 2")
(def +test-list-id-3+ "c8bb5589-1bf1-49a6-994d-0510b7e2e42e")

(def +test-list-name-3+ "Test List 3")

(def +test-new-list-name+ "New List for Testing")

;;; ---------------------------------------------------------------------
;;; setup and teardown
;;; ---------------------------------------------------------------------

(defn setup-test-data []
  (println "setting up test data...")
  (let [test-collection-1 (model/make-collection-document :name +test-collection-name-1+
                                                          :owner +test-user-id-1+
                                                          :id +test-collection-id-1+)
        test-collection-2 (model/make-collection-document :name +test-collection-name-2+
                                                          :owner +test-user-id-1+
                                                          :id +test-collection-id-2+)
        test-list-1 (model/make-list-document :name +test-list-name-1+
                                              :owner +test-user-id-1+
                                              :id +test-list-id-1+
                                              :collection +test-collection-id-1+)
        test-list-2 (model/make-list-document :name +test-list-name-2+
                                              :owner +test-user-id-1+
                                              :id +test-list-id-2+
                                              :collection +test-collection-id-1+)
        test-list-3 (model/make-list-document :name +test-list-name-3+
                                              :owner +test-user-id-1+
                                              :id +test-list-id-3+
                                              :collection +test-collection-id-1+)]
    (model/assert-collection! test-collection-1)
    (model/assert-collection! test-collection-2)
    (model/assert-list! test-list-1)
    (model/assert-list! test-list-2)
    (model/assert-list! test-list-3))
  ;;; wait after setup to make sure DB's API returns consistent results
  (Thread/sleep 1000))

;;; (setup-test-data)
;;; (teardown-test-data)

(defn teardown-test-data []
  (println "deleting test data...")
  ;;; wait before teardown to make sure DB's API returns consistent results
  (Thread/sleep 1000)
  (let [test-documents1 (couchio/find-objects (config/delectus-content-bucket) []
                                              {"owner" +test-user-id-1+})
        test-documents2 (couchio/find-objects (config/delectus-content-bucket) []
                                             {"owner" +test-user-id-2+})]
    (doseq [doc test-documents1]
      (couchio/remove-document! (config/delectus-content-bucket)
                                (.get doc +id-attribute+)))
    (doseq [doc test-documents2]
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
    (let [found-user (api/authenticate +test-user-id-1+ +test-user-password-1+)]
      (is (and found-user
               (= +user-type+ (get found-user +type-attribute+)))
          "found-user should be a user object"))))

(deftest login-test
  (testing "/api/user/login"
    (let [found-user (api/login +test-user-email-1+ +test-user-password-1+)]
      (is (and found-user
               (= +user-type+ (get found-user +type-attribute+)))
          "found-user should be a user object"))))

(deftest userid-test
  (testing "/api/user/userid"
    (let [found-id (api/userid +test-user-email-1+)]
      (is found-id "found-id should be a user ID string")
      (is (= found-id +test-user-id-1+)
          "found-id should equal to the standard test-user1 ID"))))

(deftest userdata-test
  (testing "/api/user/userdata"
    (let [data (api/userdata +test-user-id-1+ [])]
      (is (= (get data +id-attribute+) +test-user-id-1+)
          "userid should be the standard test-user ID string")
      (is (= (get data +email-attribute+) +test-user-email-1+)
          "email should be the standard test-user email string"))))

;;; ---------------------------------------------------------------------
;;; Collection tests
;;; ---------------------------------------------------------------------

(deftest collections-test
  (testing "/api/collection/collections"
    (let [collection-maps (api/collections +test-user-id-1+ [])]
      (is (> (count collection-maps) 0)
          (str "there should be several test collections but found " (count collection-maps)))
      (is (every? #(= +collection-type+ (get % +type-attribute+)) collection-maps)
          "all found objects should be collections"))))

(deftest collection-with-id-test
  (testing "/api/collection/collection_with_id"
    (let [collection (api/collection-with-id +test-user-id-1+ +test-collection-id-1+ [])]
      (is (= (get collection +owner-attribute+)
             +test-user-id-1+)
          "the collection's owner should be the standard test-user1 ID")
      (is (= (get collection +name-attribute+)
             +test-collection-name-1+)
          "the collection's name should be the standard test-user1 name"))))

(deftest collection-name-test
  (testing "/api/collection/collection_name"
    (let [name (api/collection-name +test-user-id-1+ +test-collection-id-1+)]
      (is (= name +test-collection-name-1+)
          "the collection's name should be equal to +test-collection-name-1+"))))

(deftest find-collections-with-name-test
  (testing "/api/collection/find_collections_with_name"
    (let [found (api/find-collections-with-name +test-user-id-1+ +test-collection-name-1+ [])]
      (if (= 1 (count found))
        (let [collection (first found)]
          (is (= +test-collection-id-1+ (get collection +id-attribute+))
              (str "the found collection should have the ID " +test-collection-id-1+)))
        (is (= 1 (count found))
            "should be one collection found")))))

(deftest rename-collection-test
  (testing "/api/collection/rename_collection"
    (let [found-name (api/collection-name +test-user-id-1+ +test-collection-id-1+)]
      (is (= found-name +test-collection-name-1+)
          (str "found-name should initially be equal to " +test-collection-name-1+))
      (api/rename-collection +test-user-id-1+ +test-collection-id-1+ +test-collection-alt-name-1+)
      (let [found-name (api/collection-name +test-user-id-1+ +test-collection-id-1+)]
        (is (= found-name +test-collection-alt-name-1+)
            (str "found-name should now be equal to " +test-collection-alt-name-1+)))
      (api/rename-collection +test-user-id-1+ +test-collection-id-1+ +test-collection-name-1+))))

(deftest new-collection-test
  (testing "/api/collection/new_collection"
    (let [newcollid (api/new-collection +test-user-id-1+ +test-new-collection-name+)]
      ;; wait to ensure db is consistent before checking
      (Thread/sleep 500)
      (let [foundids (api/find-collections-with-name +test-user-id-1+ +test-new-collection-name+ [])]
        (is (> (count foundids) 0) "expected one found collection"))
      (let [foundcoll (api/collection-with-id +test-user-id-1+ newcollid [])
            foundname (get foundcoll +name-attribute+)]
        (is (= foundname +test-new-collection-name+)
            (str "expected the found collection's name to be " +test-new-collection-name+))))))

;;; delete-collection-test tests:
;;;   api/collection-deleted?
;;;   api/delete-collection
;;;   api/undelete-collection
(deftest delete-collection-test
  (testing "/api/collection/delete_collection"
    (is (not (api/collection-deleted? +test-user-id-1+ +test-collection-id-2+))
        "expected +test-collection-id-2+ to initially be marked not deleted")
    (api/delete-collection +test-user-id-1+ +test-collection-id-2+)
    (is (api/collection-deleted? +test-user-id-1+ +test-collection-id-2+)
        "expected +test-collection-id-2+ to now be marked deleted")
    (api/undelete-collection +test-user-id-1+ +test-collection-id-2+)
    (is (not (api/collection-deleted? +test-user-id-1+ +test-collection-id-2+))
        "expected +test-collection-id-2+ to now be marked not deleted")))

(deftest collection-lists-test
  (testing "/api/collection/collection_lists"
    (let [foundlists1 (api/collection-lists  +test-user-id-1+ +test-collection-id-1+ [])]
      (is (> (count foundlists1) 0) "expected to find some lists in +test-collection-id-1+"))
    (let [foundlists2 (api/collection-lists +test-user-id-1+ +test-collection-id-2+ [])]
      (is (not (> (count foundlists2) 0)) "expected to find no lists in +test-collection-id-2+"))))

;;; ---------------------------------------------------------------------
;;; List tests
;;; ---------------------------------------------------------------------

(deftest lists-test
  (testing "/api/list/lists"
    (let [found-lists1 (api/lists +test-user-id-1+ [])]
      (is (> (count found-lists1) 0)
          "expected several lists belonging to +test-user-id-1+"))
    (let [found-lists2 (api/lists +test-user-id-2+ [])]
      (is (<= (count found-lists2) 0)
          "expected no lists belonging to +test-user-id-2+"))))

;;; move-list-to-collection-test tests:
;;;   api/move-list-to-collection
;;;   api/make-list-uncollected
(deftest move-list-to-collection-test
  (testing "/api/list/move_list_to_collection"
    (is (= (api/list-collection +test-user-id-1+ +test-list-id-1+)
           +test-collection-id-1+)
        "step 1: expected +test-list-id-1+ to be in collection +test-collection-id-1+")
    (api/make-list-uncollected +test-user-id-1+ +test-list-id-1+)
    (is (nil? (api/list-collection +test-user-id-1+ +test-list-id-1+))
        "step 2: expected +test-list-id-1+ to be uncollected")
    (api/move-list-to-collection +test-user-id-1+ +test-list-id-1+ +test-collection-id-2+)
    (is (= (api/list-collection +test-user-id-1+ +test-list-id-1+)
           +test-collection-id-2+)
        "step3: expected +test-list-id-1+ to be in collection +test-collection-id-2+")
    (api/move-list-to-collection +test-user-id-1+ +test-list-id-1+ +test-collection-id-1+)
    (is (= (api/list-collection +test-user-id-1+ +test-list-id-1+)
           +test-collection-id-1+)
        "step 4:expected +test-list-id-1+ to be in collection +test-collection-id-1+")))

(deftest list-with-id-test
  (testing "/api/list/list_with_id"
    (let [found-list (api/list-with-id +test-user-id-1+ +test-list-id-1+ [])]
      (is (= +list-type+ (get found-list +type-attribute+))
          "expected an object of type delectus_list")
      (is (= +test-list-name-1+ (get found-list +name-attribute+))
          (str "expected a list named " +test-list-name-1+)))))

(deftest list-name-test
  (testing "/api/list/list-name"
    (let [found-name (api/list-name +test-user-id-1+ +test-list-id-1+)]
      (is (= found-name +test-list-name-1+)
          (str "expected found name to be " +test-list-name-1+)))))

(deftest list-collection-test
  (testing "/api/list/list-name"
    (let [found-collection-id (api/list-collection +test-user-id-1+ +test-list-id-3+)]
      (is (= found-collection-id +test-collection-id-1+)
          (str "expected found collection ID to be " +test-collection-id-1+)))))


(deftest find-lists-with-name-test
  (testing "/api/collection/find_lists_with_name"
    (let [found (api/find-lists-with-name +test-user-id-1+ +test-list-name-1+ [])]
      (if (= 1 (count found))
        (let [found-list (first found)]
          (is (= +test-list-id-1+ (get found-list +id-attribute+))
              (str "the found list should have the ID " +test-list-id-1+)))
        (is (= 1 (count found))
            "should be one list found")))))


(deftest rename-list-test
  (testing "/api/list/rename_list"
    (let [found-name (api/list-name +test-user-id-1+ +test-list-id-1+)]
      (is (= found-name +test-list-name-1+)
          (str "found-name should initially be equal to " +test-list-name-1+))
      (api/rename-list +test-user-id-1+ +test-list-id-1+ +test-list-alt-name-1+)
      (let [found-name (api/list-name +test-user-id-1+ +test-list-id-1+)]
        (is (= found-name +test-list-alt-name-1+)
            (str "found-name should now be equal to " +test-list-alt-name-1+)))
      (api/rename-list +test-user-id-1+ +test-list-id-1+ +test-list-name-1+))))


(deftest new-list-test
  (testing "/api/list/new_list"
    (let [newlistid (api/new-list +test-user-id-1+ +test-new-list-name+)]
      ;; wait to ensure db is consistent before checking
      (Thread/sleep 500)
      (let [foundids (api/find-lists-with-name +test-user-id-1+ +test-new-list-name+ [])]
        (is (> (count foundids) 0) "expected one found list"))
      (let [foundlist (api/list-with-id +test-user-id-1+ newlistid [])
            foundname (get foundlist +name-attribute+)]
        (is (= foundname +test-new-list-name+)
            (str "expected the found list's name to be " +test-new-list-name+))))))

;;; delete-list-test tests:
;;;   api/list-deleted?
;;;   api/delete-list
;;;   api/undelete-list
(deftest delete-list-test
  (testing "/api/list/delete_list"
    (is (not (api/list-deleted? +test-user-id-1+ +test-list-id-2+))
        "expected +test-list-id-2+ to initially be marked not deleted")
    (api/delete-list +test-user-id-1+ +test-list-id-2+)
    (is (api/list-deleted? +test-user-id-1+ +test-list-id-2+)
        "expected +test-list-id-2+ to now be marked deleted")
    (api/undelete-list +test-user-id-1+ +test-list-id-2+)
    (is (not (api/list-deleted? +test-user-id-1+ +test-list-id-2+))
        "expected +test-list-id-2+ to now be marked not deleted")))
