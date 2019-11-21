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
          (pp/cl-format nil "found-collections should be a list of collection objects, but found ~S"
                        found-collections)))))


(deftest collection-with-id-test
  (testing "collection-with-id"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         found-collection (collection-with-id user-id +stable-test-collection-id+)]
      (is (and (not (nil? found-collection))
               (instance? JsonObject found-collection)
               (couchio/json-object-type? found-collection +collection-type+))
          (pp/cl-format nil "found-collection should be a collection object, but found ~S"
                        found-collection)))))


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

(deftest lists-test
  (testing "lists"
    (let [email (:delectus-test-user (config/delectus-configuration))
          user-id (userid email)
          found-lists (lists user-id)]
      (is (and found-lists
               (not (empty? found-lists))
               (every? #(and (instance? JsonObject %)
                             (couchio/json-object-type? % +list-type+))
                       found-lists))
          (pp/cl-format nil "found-lists should be a list of List objects, but found ~S"
                        found-lists)))))


(deftest list-with-id-test
  (testing "list-with-id"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         found-list (list-with-id user-id +stable-test-list-0-id+)]
      (is (and (not (nil? found-list))
               (instance? JsonObject found-list)
               (couchio/json-object-type? found-list +list-type+))
          (pp/cl-format nil "found-list should be a List object, but found ~S"
                        found-list)))))


(deftest list-name-test
  (testing "list-name"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         found-list (list-with-id user-id +stable-test-list-0-id+)
         found-name (.get found-list +name-attribute+)]
      (is (and (string? found-name)
               (= found-name +stable-test-list-0-name+))
          (pp/cl-format nil "found-name should be ~S but found ~S"
                        +stable-test-list-0-name+
                        found-name)))))


(deftest list-named-test
  (testing "list-named"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)
         found-list (list-named user-id +stable-test-list-1-name+)
         found-id (.get found-list +id-attribute+)]
      (is (and (string? found-id)
               (= found-id +stable-test-list-1-id+))
          (pp/cl-format nil "found-id should be ~S but found ~S"
                        +stable-test-list-1-id+ found-id)))))

(deftest rename-list-test
  (testing "rename-list"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)]
      ;; rename the test list
      (rename-list user-id +stable-test-list-1-id+ +stable-test-list-1-alternate-name+)
      (let [found-list (couchio/get-list +stable-test-list-1-id+)
            found-name (.get found-list +name-attribute+)]
        (is (and (string? found-name)
                 (= found-name +stable-test-list-1-alternate-name+))
            (pp/cl-format nil "found-name should be ~S, but found ~S"
                          +stable-test-list-1-alternate-name+
                          found-name)))
      ;; change the name back
      (rename-list user-id +stable-test-list-1-id+ +stable-test-list-1-name+)
      (let [found-list (couchio/get-list +stable-test-list-1-id+)
            found-name (.get found-list +name-attribute+)]
        (is (and (string? found-name)
                 (= found-name +stable-test-list-1-name+))
            (pp/cl-format nil "found-name should be ~S, but found ~S"
                          +stable-test-list-1-name+
                          found-name))))))


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
          (pp/cl-format nil "test value should be a List object, but found ~S" the-list)))))


(deftest mark-list-deleted-test
  (testing "mark-list-deleted and list-deleted?"
    (let
        [email (:delectus-test-user (config/delectus-configuration))
         user-id (userid email)]
      (mark-list-deleted user-id +stable-test-list-0-id+ true)
      (let [found-list (couchio/get-list +stable-test-list-0-id+)]
        (is (not (nil? found-list)) "test list is nil")
        (is (instance? JsonObject found-list)
            (pp/cl-format nil "test value should be a List object, but found ~S" found-list))
        (is (list-deleted? user-id +stable-test-list-0-id+) "test list should be deleted, but is not"))
      (mark-list-deleted user-id +stable-test-list-0-id+ false)
      (let [found-list (couchio/get-list +stable-test-list-0-id+)]
        (is (not (nil? found-list)) "test list is nil")
        (is (instance? JsonObject found-list)
            (pp/cl-format nil "test value should be a List object, but found ~S" found-list))
        (is (not (collection-deleted? user-id +stable-test-collection-id+))
            "test list should not be deleted, but is")))))


;;; list-column-test
;;; ----------------
;;; testing column functions:
;;; list-columns
;;; column-with-id
;;; column-name
;;; column-named
;;; column-deleted?
;;; mark-column-deleted
;;; new-column
;;; rename-column

(deftest list-columns-test
  (testing "list column functions"
    (let [email (:delectus-test-user (config/delectus-configuration))
          user-id (userid email)
          found-list (couchio/get-list +stable-test-list-0-id+)]
      (new-column :owner-id user-id :list-id +stable-test-list-0-id+ :name +stable-test-column-name-a+)
      (new-column :owner-id user-id :list-id +stable-test-list-0-id+ :name +stable-test-column-name-b+)
      (let [found-columns (list-columns user-id +stable-test-list-0-id+)]
        (is (not (nil? found-columns)) "found-columns is nil")
        (is (instance? JsonObject found-columns)
            (pp/cl-format nil "found-columns should be a JsonObject, but found ~S" found-columns))
        (let [column-keys (into [] (.getNames found-columns))]
          (is (= 2 (count column-keys))
              (pp/cl-format nil "found-columns should have 2 members, but found ~S" found-columns)))
        (let [column0 (column-with-id :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0")
              column-zero (column-named :owner-id user-id :list-id +stable-test-list-0-id+
                                        :column-name +stable-test-column-name-a+)
              found-id (.get column0 +id-attribute+)
              found-name (column-name :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0")
              found-deleted? (.get column0 +deleted-attribute+)]
          (let [column0-id (.get column0 +id-attribute+)
                column-zero-id (.get column-zero +id-attribute+)]
            (is (= column0-id column-zero-id)
                (pp/cl-format nil "column0-id and column-zero-id differ: column0: ~S; column-zero: ~S"
                              column0-id column-zero-id)))
          (is (= found-id "0")
              (pp/cl-format nil "found-id should be ~S, but found ~S"
                            "0" found-id))
          (is (= found-name +stable-test-column-name-a+)
              (pp/cl-format nil "found-name should be ~S, but found ~S"
                            +stable-test-column-name-a+ found-name))
          (is (= found-deleted? false)
              (pp/cl-format nil "found-deleted? should be false, but found ~S" found-name)))
        ;; mark column 0 deleted
        (mark-column-deleted user-id +stable-test-list-0-id+ "0" true)
        (is (column-deleted? :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0")
            (pp/cl-format nil "column0 should be deleted, but it isn't."))
        ;; mark column 0 undeleted
        (mark-column-deleted user-id +stable-test-list-0-id+ "0" false)
        (is (not (column-deleted? :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0"))
            (pp/cl-format nil "column0 shouldn't be deleted, but it is."))
        ;; test renaming the column
        (let [old-name (column-name :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0")
              new-name "Frob"]
          (rename-column user-id +stable-test-list-0-id+ "0" new-name)
          (let [found-name (column-name :owner-id user-id :list-id +stable-test-list-0-id+ :column-id "0")]
            (is (= found-name new-name)
              (pp/cl-format nil "column0 name should be ~S but found ~S" new-name found-name))))))))


;;; list-items-test
;;; ---------------
;;; testing items functions:
;;; list-items
;;; item-with-id
;;; new-item
;;; mark-item-deleted
;;; item-deleted?
;;; item-column-value
;;; set-item-column-value

(deftest list-items-test
  (testing "list items functions"
    (let [email (:delectus-test-user (config/delectus-configuration))
          user-id (userid email)
          found-list (couchio/get-list +stable-test-list-2-id+)]
      ;; create columns
      (new-column :owner-id user-id :list-id +stable-test-list-2-id+ :name +stable-test-column-name-a+)
      (new-column :owner-id user-id :list-id +stable-test-list-2-id+ :name +stable-test-column-name-b+)
      ;; check columns
      (let [found-columns (list-columns user-id +stable-test-list-2-id+)]
        (is (not (nil? found-columns)) "found-columns is nil")
        (is (instance? JsonObject found-columns)
            (pp/cl-format nil "found-columns should be a JsonObject, but found ~S" found-columns))
        (let [column-keys (into [] (.getNames found-columns))]
          (is (= 2 (count column-keys))
              (pp/cl-format nil "found-columns should have 2 members, but found ~S" found-columns))))
      ;; create items
      (new-item :owner-id user-id :list-id +stable-test-list-2-id+)
      (new-item :owner-id user-id :list-id +stable-test-list-2-id+)
      ;; check items
      (let [found-items (list-items user-id +stable-test-list-2-id+)]
        (is (not (nil? found-items)) "found-items is nil")
        (is (instance? JsonObject found-items)
            (pp/cl-format nil "found-items should be a JsonObject, but found ~S" found-items))
        (let [item-keys (into [] (.getNames found-items))]
          (is (= 2 (count item-keys))
              (pp/cl-format nil "found-items should have 2 members, but found ~S" found-items))))
      ;; test a single item
      (let [found-item (item-with-id user-id +stable-test-list-2-id+ "0")]
        (is (not (nil? found-item)) "found-item is nil")
        (is (instance? JsonObject found-item)
            (pp/cl-format nil "found-item should be a JsonObject, but found ~S" found-item))
        ;; check that it exists and has two fields
        (let [found-fields (.get found-item +fields-attribute+)
              field-keys (into [] (.getNames found-fields))]
          (is (= 2 (count field-keys))
              (pp/cl-format nil "field-keys should have 2 members, but found ~S" field-keys)))
        ;; delete and undelete it
        (mark-item-deleted user-id +stable-test-list-2-id+ "0" true)
        (is (item-deleted? user-id +stable-test-list-2-id+ "0")
            "item 0 should be marked deleted, but isn't")
        (mark-item-deleted user-id +stable-test-list-2-id+ "0" false)
        (is (not (item-deleted? user-id +stable-test-list-2-id+ "0"))
            "item 0 shouldn't be marked deleted, but is")
        ;; write and then read back test data
        (let [testval1 "supercalifragilisticexpialidocious"
              testval2 12345]
          (set-item-column-value user-id +stable-test-list-2-id+ "0" "0" testval1)
          (set-item-column-value user-id +stable-test-list-2-id+ "1" "1" testval2)
          (let [foundval1 (item-column-value user-id +stable-test-list-2-id+ "0" "0")
                foundval2 (item-column-value user-id +stable-test-list-2-id+ "1" "1")]
            (is (= foundval1 testval1)
                (pp/cl-format nil "field 0,0 should have value ~S, but found ~S" testval1 foundval1))
            (is (= foundval2 testval2)
                (pp/cl-format nil "field 1,1 should have value ~S, but found ~S" testval2 foundval2))))))))
