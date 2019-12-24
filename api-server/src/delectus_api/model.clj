(ns delectus-api.model
  (:require
   [buddy.hashers :as hashers]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.couchio :as couchio])
  (:import
   (com.couchbase.client.java.document JsonDocument)))


;;; ---------------------------------------------------------------------
;;; ensuring type and attribute invariants
;;; ---------------------------------------------------------------------

(defmacro ensure-user-exists [userid]
  `(if (user-exists? ~userid)
     ~userid
     (throw (ex-info "No such user"
                     {:cause :user-not-found
                      :userid ~userid}))))

(defmacro ensure-user [userid]
  (let [found-user (gensym)]
    `(let [~found-user (get-user ~userid)]
       (or ~found-user
           (throw (ex-info "No such user"
                           {:cause :user-not-found
                            :userid ~userid}))))))

(defmacro ensure-collection-exists [collectionid]
  `(if (collection-exists? ~collectionid)
     ~collectionid
     (throw (ex-info "No such collection"
                     {:cause :collection-not-found
                      :collectionid ~collectionid}))))

(defmacro ensure-collection [collectionid]
  (let [found-collection (gensym)]
    `(let [~found-collection (get-collection ~collectionid)]
       (or ~found-collection
           (throw (ex-info "No such collection"
                           {:cause :collection-not-found
                            :collectionid ~collectionid}))))))

(defmacro ensure-list-exists [listid]
  `(if (list-exists? ~listid)
     ~listid
     (throw (ex-info "No such list"
                     {:cause :list-not-found
                      :listid ~listid}))))

(defmacro ensure-list [listid]
  (let [found-list (gensym)]
    `(let [~found-list (get-list ~listid)]
       (or ~found-list
           (throw (ex-info "No such list"
                           {:cause :list-not-found
                            :listid ~listid}))))))



(defmacro ensure-owner [objectid userid]
  (let [found-owner (gensym)]
    `(let [~found-owner (couchio/get-object-attribute (config/delectus-content-bucket)
                                                      ~objectid +owner-attribute+)]
       (if (and ~found-owner
                (= ~found-owner ~userid))
         ~found-owner
         (throw (ex-info "Wrong owner ID"
                         {:cause :wrong-owner
                          :expected ~userid
                          :found ~found-owner}))))))

;;; ---------------------------------------------------------------------
;;; model type-checks
;;; ---------------------------------------------------------------------

(defmacro ensure-document-type [json-doc type-string]
  `(if (instance? JsonDocument ~json-doc)
     (let [found-type# (.get (.content ~json-doc) +type-attribute+)]
       (or (and (= ~type-string found-type#))
           (throw (ex-info "Wrong document type"
                           {:cause :wrong-document-type
                            :expected-type ~type-string
                            :found-type found-type#}))))
     (throw (ex-info "Wrong object type"
                     {:cause :wrong-object-type
                      :expected-type JsonDocument
                      :found-type (type ~json-doc)}))))

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
  (ensure-document-type userdoc +user-type+)
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
  (ensure-document-type collectiondoc +collection-type+)
  (let [content-bucket (config/delectus-content-bucket)
        upserted-doc (.upsert content-bucket collectiondoc)]
    upserted-doc))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingscol (make-collection-document :name "Things" :owner $mikelid))
;;; (ensure-document-type $thingscol +collection-type+)
;;; (def $upserted-col (assert-collection! $thingscol))

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
  (ensure-document-type listdoc +list-type+)
  (let [content-bucket (config/delectus-content-bucket)
        upserted-doc (.upsert content-bucket listdoc)]
    upserted-doc))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $movies (make-list-document :name "Movies" :owner $mikelid))
;;; (ensure-document-type $movies +list-type+)
;;; (def $upserted-ls (assert-list! $movies))

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

;;; list columns
;;; ---------------------------------------------------------------------

(defn get-list-columns [listid]
  (let [found-list (ensure-list listid)]
    (.get found-list +columns-attribute+)))

;;; TODO: use a N1QL query to return items whose list is identified by the the supplied listid 
(defn get-list-items [listid]
  )

;;; (def $moviesid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf")
;;; (get-list-columns $moviesid)
;;; (get-list-items $moviesid)

