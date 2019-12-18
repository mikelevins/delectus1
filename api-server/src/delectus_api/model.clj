(ns delectus-api.model
  (:require
   [buddy.hashers :as hashers]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.couchio :as couchio]))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn make-user-document [& {:keys [id email name password-hash enabled]
                             :or {id (makeid)
                                  email nil
                                  name nil
                                  password-hash nil
                                  enabled true}}]
  (errors/error-if-nil email "Missing email parameter" {:context "make-user-document"})
  (let [obj-map {+type-attribute+ +user-type+
                 +id-attribute+ id
                 +email-attribute+ email
                 +name-attribute+ name
                 +password-hash-attribute+ password-hash
                 +enabled-attribute+ enabled}]
    (couchio/make-json-document id obj-map)))


(defn assert-user! [userdoc]
  (let [users-bucket (config/delectus-users-bucket)
        upserted-doc (.upsert users-bucket userdoc)]
    upserted-doc))

;;; (def $conf (config/delectus-configuration))
;;; (def $username (:delectus-test-user $conf))
;;; (def $password-hash (hashers/derive (:delectus-test-user-password $conf)))
;;; (def $newuser (make-user-document :email $username :name "Joe Test" :password-hash $password-hash))
;;; (def $upserted-user (assert-new-user $newuser))

(defn user-exists? [userid]
  (and (couchio/id-exists? (config/delectus-users-bucket) userid)
       (= +user-type+
          (couchio/get-object-attribute (config/delectus-users-bucket)
                                        userid +type-attribute+))))

(defn get-user [userid]
  (or (and userid
           (let [candidate (couchio/get-document (config/delectus-users-bucket) userid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (couchio/json-object-type? obj +user-type+)
                      obj
                      nil)))))
      nil))

;;; finding registered users
;;; ---------------------------------------------------------------------

(defn email->user [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (first found))))

(defn email->userid [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (.get (first found) +id-attribute+))))

(defn id->user [userid]
  (get-user userid))

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn make-collection-document [& {:keys [id name owner-id lists deleted]
                                   :or {id (makeid)
                                        name nil
                                        owner-id nil
                                        lists nil
                                        deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-collection-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-collection-document"})
  (let [obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ lists
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-collection-document :name "Things" :owner-id $mikelid)q

(defn collection-exists? [collectionid]
  (and (couchio/id-exists? (config/delectus-content-bucket) collectionid)
       (= +collection-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +type-attribute+))))

(defn get-collection [collectionid]
  (or (and collectionid
           (let [candidate (couchio/get-document (config/delectus-content-bucket) collectionid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (couchio/json-object-type? obj +collection-type+)
                      obj
                      nil)))))
      nil))

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn make-list-document [& {:keys [id name owner-id collection-id columns items deleted]
                             :or {id (makeid)
                                  name nil
                                  owner-id nil
                                  collection-id nil
                                  columns nil
                                  items nil
                                  deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-list-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +list-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +collection-attribute+ collection-id
                 +columns-attribute+ columns
                 +items-attribute+ items
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

(defn list-exists? [listid]
  (and (couchio/id-exists? (config/delectus-content-bucket) listid)
       (= +list-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        listid +type-attribute+))))

(defn get-list [listid]
  (or (and listid
           (let [candidate (couchio/get-document (config/delectus-content-bucket) listid)]
             (and candidate
                  (let [obj (.content candidate)]
                    (if (couchio/json-object-type? obj +list-type+)
                      obj
                      nil)))))
      nil))

