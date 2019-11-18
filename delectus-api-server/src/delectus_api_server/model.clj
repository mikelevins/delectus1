(ns delectus-api-server.model
  (:require
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :refer :all]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.itemid :as itemid]
   [delectus-api-server.couchio :as couchio]))

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; Delectus-specific data structures

;;; ---------------------------------------------------------------------
;;; User, Collection, and List
;;; ---------------------------------------------------------------------

(defn make-user-document [& {:keys [id email name password-hash enabled metadata]
                             :or {id (makeid)
                                  email nil
                                  name nil
                                  password-hash nil
                                  enabled true
                                  metadata {}}}]
  (errors/error-if-nil email "Missing email parameter" {:context "make-user-document"})
  (let [obj-map {+type-attribute+ +user-type+
                 +id-attribute+ id
                 +email-attribute+ email
                 +name-attribute+ name
                 +password-hash-attribute+ password-hash
                 +enabled-attribute+ enabled
                 +delectus-metadata-attribute+ metadata}]
    (couchio/make-json-document id obj-map)))

;;; (make-user-document :email "mikel@evis.net")

(defn make-collection-document [& {:keys [id name owner-id lists deleted metadata]
                                   :or {id (makeid)
                                        name nil
                                        owner-id nil
                                        ;; lists is a set of list-ids
                                        ;; we represent that as a JSON object
                                        ;; the keys are the lists, the vals are ignored
                                        lists {}
                                        deleted false
                                        metadata {}}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-collection-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-collection-document"})
  (let [obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ lists
                 +deleted-attribute+ deleted
                 +delectus-metadata-attribute+ metadata}]
    (couchio/make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-collection-document :name "Random stuff" :owner-id $mikelid)

(defn make-default-collection [& {:keys [id owner-id metadata]
                                  :or {id (makeid)
                                       owner-id nil
                                       metadata {}}}]
  (let [obj-id (makeid)
        obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ obj-id
                 +name-attribute+ +standard-default-collection-name+
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ {}
                 +deleted-attribute+ false
                 +delectus-metadata-attribute+ (merge +standard-delectus-metadata+
                                                      {"default_collection" true}
                                                      metadata)}]
    (couchio/make-json-document obj-id obj-map)))

(defn make-list-document [& {:keys [id name owner-id columns items deleted metadata]
                             :or {id (makeid)
                                  name nil
                                  owner-id nil
                                  columns {}
                                  items {}
                                  deleted false
                                  metadata {}}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-list-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +list-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +columns-attribute+ columns
                 +items-attribute+ items
                 +deleted-attribute+ deleted
                 +delectus-metadata-attribute+ metadata}]
    (couchio/make-json-document id obj-map)))


;;; ---------------------------------------------------------------------
;;; Column and Row
;;; ---------------------------------------------------------------------

(defn make-column-object [& {:keys [id name deleted]
                             :or {id (itemid/first-itemid)
                                  name nil
                                  deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-column-object"})
  (let [obj-map {+id-attribute+ id
                 +name-attribute+ name
                 +deleted-attribute+ deleted}]
    (couchio/make-json-object obj-map)))

;;; (make-column-object :name "Title")

(defn make-row-object [& {:keys [id fields deleted]
                          :or {id (itemid/first-itemid)
                               fields {}
                               deleted false}}]
  (errors/error-if-nil id "Missing id parameter" {:context "make-row-object"})
  (let [obj-map {+id-attribute+ id
                 +fields-attribute+ fields
                 +deleted-attribute+ deleted}]
    (couchio/make-json-object obj-map)))

;;; (make-row-object)
;;; (make-row-object :fields {"name" "Fred"})

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn email->user [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (first found))))

;;; (email->user "mikel@evins.net")
;;; (email->user "greer@evins.net")
;;; (email->user "nobody@nowhere.net")

(defn email->userid [email]
  (let [found-user (email->user email)]
    (if found-user
      (.get found-user "id")
      nil)))

;;; (email->userid "mikel@evins.net")
;;; (email->userid "greer@evins.net")
;;; (email->userid "nobody@nowhere.net")

(defn user-exists? [userid]
  (let [bucket (config/delectus-users-bucket)]
    (and (couchio/id-exists? bucket userid)
         (= +user-type+ (couchio/get-object-type bucket userid)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (user-exists? $mikelid)
;;; (user-exists? "NO!")
;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (user-exists? $default-collection-id)

(defn get-user-email [userid]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context get-user-email})
    (couchio/get-object-attribute bucket userid +email-attribute+)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-email $mikelid)
;;; (get-user-email "NOPE!")

(defn set-user-email! [userid new-email]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context set-user-email!})
    (couchio/upsert-object-attribute! bucket userid +email-attribute+ new-email)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-email $mikelid)
;;; (set-user-email! $mikelid "mikel@evins.net")

(defn get-user-name [userid]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context get-user-name})
    (couchio/get-object-attribute bucket userid +name-attribute+)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-name $mikelid)

(defn set-user-name! [userid new-name]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context set-user-name!})
    (couchio/upsert-object-attribute! bucket userid +name-attribute+ new-name)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-name $mikelid)
;;; (set-user-name! $mikelid "mikel evins")

(defn get-user-password-hash [userid]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context get-user-password-hash})
    (couchio/get-object-attribute bucket userid +password-hash-attribute+)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-password-hash $mikelid)

(defn set-user-password-hash! [userid new-hash]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context set-user-password-hash!})
    (couchio/upsert-object-attribute! bucket userid +password-hash-attribute+ new-hash)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $hash (get-user-password-hash $mikelid))
;;; (set-user-password-hash! $mikelid "NOPE!")
;;; (set-user-password-hash! $mikelid $hash)

(defn get-user-enabled [userid]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context get-user-enabled})
    (couchio/get-object-attribute bucket userid +enabled-attribute+)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-enabled $mikelid)

(defn set-user-enabled! [userid enabled?]
  (let [bucket (config/delectus-users-bucket)]
    (errors/error-if-not (user-exists? userid) "No such user" {:context set-user-enabled!})
    (couchio/upsert-object-attribute! bucket userid +enabled-attribute+ enabled?)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (get-user-enabled $mikelid)
;;; (set-user-enabled! $mikelid true)

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn id->collection [collection-id]
  (couchio/get-collection collection-id))

(defn name->collection [userid name]
  (let [found (couchio/find-objects
               (config/delectus-content-bucket) []
               {+type-attribute+ +collection-type+
                +owner-id-attribute+ userid
                +name-attribute+ name})]
    (if (empty? found)
      nil
      (first found))))

(defn collection-exists? [collection-id]
  (let [bucket (config/delectus-content-bucket)]
    (and (couchio/id-exists? bucket collection-id)
         (= +collection-type+ (couchio/get-object-type bucket collection-id)))))

;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (collection-exists? $default-collection-id)
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-exists? $mikelid)
;;; (collection-exists? "NO!")

(defn get-collection-name [collection-id]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (collection-exists? collection-id) "No such collection"
                         {:id collection-id :context "get-collection-name"})
    (couchio/get-object-attribute bucket collection-id +name-attribute+)))

;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection-name $default-collection-id)
;;; (get-collection-name "NOPE!")

(defn get-collection-owner-id [collection-id]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (collection-exists? collection-id) "No such collection"
                         {:id collection-id :context "get-collection-owner-id"})
    (couchio/get-object-attribute bucket collection-id +owner-id-attribute+)))

;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection-owner-id $default-collection-id)

;;; TODO: replace with accessors that return individual list entries
(defn get-collection-lists [collection-id]
  (errors/error-not-implemented 'get-collection-lists))

;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection-lists $default-collection-id)

(defn get-collection-deleted [collection-id]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (collection-exists? collection-id) "No such collection"
                         {:id collection-id :context "get-collection-deleted"})
    (couchio/get-object-attribute bucket collection-id +deleted-attribute+)))

;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (get-collection-deleted $default-collection-id)

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn id->list [list-id]
  (couchio/get-list list-id))

(defn name->list [userid name]
  (let [found (couchio/find-objects
               (config/delectus-content-bucket) []
               {+type-attribute+ +list-type+
                +owner-id-attribute+ userid
                +name-attribute+ name})]
    (if (empty? found)
      nil
      (first found))))


(defn list-exists? [listid]
  (let [bucket (config/delectus-content-bucket)]
    (and (couchio/id-exists? bucket listid)
         (= +list-type+ (couchio/get-object-type bucket listid)))))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (list-exists? $things-id)
;;; (def $default-collection-id "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (list-exists? $default-collection-id)
;;; (list-exists? "NO!")

(defn get-list-name [listid]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (list-exists? listid) "No such list"
                         {:id listid :context "get-list-name"})
    (couchio/get-object-attribute bucket listid +name-attribute+)))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list-name $things-id)
;;; (get-list-name "NOPE!")

(defn get-list-owner-id [listid]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (list-exists? listid) "No such list"
                         {:id listid :context "get-list-owner-id"})
    (couchio/get-object-attribute bucket listid +owner-id-attribute+)))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list-owner-id $things-id)

;;; TODO: replace with accessors that return attributes of
;;; individual columns
(defn get-list-columns [listid]
  (errors/error-not-implemented 'get-list-columns))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list-columns $things-id)

;;; TODO: replace with accessors that return individual items
;;;       or their fields
(defn get-list-items [listid]
  (errors/error-not-implemented 'get-list-items))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list-items $things-id)

(defn get-list-deleted [listid]
  (let [bucket (config/delectus-content-bucket)]
    (errors/error-if-not (list-exists? listid) "No such list"
                         {:id listid :context "get-list-deleted"})
    (couchio/get-object-attribute bucket listid +deleted-attribute+)))

;;; (def $things-id "7ffa6177-a5cf-41d7-a759-6e5aa5b5f642")
;;; (get-list-deleted $things-id)

