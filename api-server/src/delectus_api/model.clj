(ns delectus-api.model
  (:require
   [buddy.hashers :as hashers]
   [clojure.pprint :refer [cl-format]]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.ensure :as ensure]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.couchio :as couchio])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)))


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
  (ensure/ensure-document-type userdoc +user-type+)
  (let [users-bucket (config/delectus-users-bucket)
        upserted-doc (.upsert users-bucket userdoc)]
    upserted-doc))

;;; (def $conf (config/delectus-configuration))
;;; (def $email (:delectus-test-user2-email $conf))
;;; (def $password-hash (hashers/derive (:delectus-test-user2-password $conf)))
;;; (def $newuser (make-user-document :email $email :name "Jane Test" :password-hash $password-hash))
;;; (def $upserted-user (assert-user! $newuser))

(defn user-exists? [userid]
  (and (couchio/id-exists? (config/delectus-users-bucket) userid)
       (= +user-type+
          (couchio/get-object-attribute (config/delectus-users-bucket)
                                        userid +type-attribute+))))

(defn get-user [userid]
  (couchio/get-object-of-type (config/delectus-users-bucket) userid +user-type+))

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

(defn make-collection-document [& {:keys [id name owner lists deleted]
                                   :or {id (makeid)
                                        name nil
                                        owner nil
                                        deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-collection-document"})
  (errors/error-if-nil owner "Missing owner parameter" {:context "make-collection-document"})
  (let [obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-attribute+ owner
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

(defn assert-collection! [collectiondoc]
  (ensure/ensure-document-type collectiondoc +collection-type+)
  (let [content-bucket (config/delectus-content-bucket)
        upserted-doc (.upsert content-bucket collectiondoc)]
    upserted-doc))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingscol (make-collection-document :name "Things" :owner $mikelid))
;;; (ensure/ensure-document-type $thingscol +collection-type+)
;;; (def $upserted-col (assert-collection! $thingscol))

(defn collection-exists? [collectionid]
  (and (couchio/id-exists? (config/delectus-content-bucket) collectionid)
       (= +collection-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +type-attribute+))))

(defn get-collection [collectionid]
  (couchio/get-object-of-type (config/delectus-content-bucket) collectionid +collection-type+))


;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn make-list-document [& {:keys [id name owner collection columns deleted]
                             :or {id (makeid)
                                  name nil
                                  owner nil
                                  collection nil
                                  columns nil
                                  deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-list-document"})
  (errors/error-if-nil owner "Missing owner parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +list-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-attribute+ owner
                 +collection-attribute+ collection
                 +columns-attribute+ columns
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))


(defn assert-list! [listdoc]
  (ensure/ensure-document-type listdoc +list-type+)
  (let [content-bucket (config/delectus-content-bucket)
        upserted-doc (.upsert content-bucket listdoc)]
    upserted-doc))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $movies (make-list-document :name "Movies" :owner $mikelid))
;;; (ensure/ensure-document-type $movies +list-type+)
;;; (def $moviesid (assert-list! $movies))

(defn list-exists? [listid]
  (and (couchio/id-exists? (config/delectus-content-bucket) listid)
       (= +list-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        listid +type-attribute+))))

(defn list-name-exists? [userid listname]
  (let [found (couchio/find-objects
               (config/delectus-content-bucket) ["id"]
               {+type-attribute+ +list-type+
                +name-attribute+ listname
                +owner-attribute+ userid})]
    (if (empty? found)
      false
      true)))

;;; (list-name-exists? $mikelid "Movies")

(defn get-list [listid]
  (couchio/get-object-of-type (config/delectus-content-bucket) listid +list-type+))


;;; list columns
;;; ---------------------------------------------------------------------

(defn get-list-columns [listid]
  (let [found-list (ensure/ensure-list listid)]
    (.get found-list +columns-attribute+)))

;;; TODO: use a N1QL query to return items whose list is identified by the the supplied listid 
(defn get-list-items [listid]
  )

;;; (def $moviesid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf")
;;; (get-list-columns $moviesid)
;;; (get-list-items $moviesid)

;;; ---------------------------------------------------------------------
;;; Items
;;; ---------------------------------------------------------------------

(defn make-item [& {:keys [id owner list fields deleted]
                         :or {id (makeid)
                              owner nil
                              list nil
                              fields nil
                              deleted false}}]
  (errors/error-if-nil owner "Missing owner parameter" {:context "make-list-document"})
  (errors/error-if-nil list "Missing list parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +item-type+
                 +id-attribute+ id
                 +owner-attribute+ owner
                 +list-attribute+ list
                 +fields-attribute+ fields
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

(defn values->item-document [userid listid vals]
  (make-item :id (makeid) :owner userid :list listid :deleted false
             :fields (let [keys (map str (range 0 (count vals)))]
                       (zipmap keys vals))))

(defn assert-item! [itemdoc]
  (ensure/ensure-document-type itemdoc +item-type+)
  (let [content-bucket (config/delectus-content-bucket)
        upserted-doc (.upsert content-bucket itemdoc)]
    upserted-doc))

(defn count-items [listid]
  (couchio/with-couchbase-exceptions-rethrown
    (let [selector (str "SELECT COUNT(*) AS `itemcount` FROM `delectus_content` "
                        "WHERE `list` = '" listid "';")
          results (.query (config/delectus-content-bucket) (N1qlQuery/simple selector))]
      (.get (.value (.get (.allRows results) 0)) "itemcount"))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (def $zipcodesid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (count-items $moviesid))
;;; (time (count-items $zipcodesid))

(defn item-exists? [itemid]
  (and (couchio/id-exists? (config/delectus-content-bucket) itemid)
       (= +item-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        itemid +type-attribute+))))

(defn get-item [itemid]
  (couchio/get-object-of-type (config/delectus-content-bucket) itemid +item-type+))

