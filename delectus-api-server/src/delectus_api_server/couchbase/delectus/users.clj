(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.utilities :refer [valid-email?]]
            [delectus-api-server.couchbase.io :as couch-io]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.couchbase.delectus.authenticatable
             :refer [Authenticatable get-login-name get-password-hash update-password-hash]]
            [delectus-api-server.couchbase.delectus.enableable :refer [Enableable enabled? enable disable]]
            [delectus-api-server.couchbase.delectus.identifiable :refer [Identifiable get-id]]
            [delectus-api-server.couchbase.delectus.typable :refer [Typable get-type]]
            [delectus-api-server.couchbase.delectus.nameable :refer [Nameable get-name rename]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; User
;;; ---------------------------------------------------------------------

(defn the-user-document-type [] "delectus_user")

(defrecord User [id type email password-hash enabled]
  Enableable
  (enabled? [data] (:enabled data))
  (enable [data] (map->User (merge data {:enabled true})))
  (disable [data] (map->User (merge data {:enabled false})))

  Identifiable
  (get-id [data] (:id data))

  Typable
  (get-type [data] (:type data))

  Nameable
  (get-name [data] (:name data))
  (rename [data new-name] (map->User (merge data {:name new-name})))

  Authenticatable
  (get-login-name [data] (:email data))
  (get-password-hash [data] (:password-hash data))
  (update-password-hash [data new-password-hash]
    (map->User (merge data {:password-hash new-password-hash})))
  
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))

  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))

  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-user [& {:keys [id email name password-hash enabled]
                    :or {id (makeid)
                         email nil
                         name nil
                         password-hash nil
                         enabled false}}]
  (when (not email)
    (throw (ex-info ":email parameter missing" {})))
  (when (not (valid-email? email))
    (throw (ex-info "invalid :email parameter" {:value email})))
  (map->User {:id id
              :type (the-user-document-type)
              :email email
              :name name
              :password-hash password-hash
              :enabled enabled}))

;;; (def $mikel-id (makeid))
;;; (def $mikel (make-user :id $mikel-id :email "mikel@evins.net"))
;;; (satisfies? Identifiable $mikel)
;;; (satisfies? Authenticatable $mikel)
;;; (get-id $mikel)
;;; (get-type $mikel)
;;; (get-login-name $mikel)
;;; (get-password-hash $mikel)
;;; (def $mikel2 (update-password-hash $mikel "foo"))
;;; (get-password-hash $mikel2)
;;; (make-couchable $mikel)
;;; (def $mikel2 (rename $mikel "mikel evins"))
;;; (make-couchable $mikel2)
;;; (to-json-object $mikel)
;;; (to-json-document $mikel $mikel-id)

;;; ---------------------------------------------------------------------
;;; Couchbase User records
;;; ---------------------------------------------------------------------

(defn delectus-users []
  (let [bucket (config/delectus-users-bucket)
        bucket-name (.name bucket)
        select-expression (cl-format nil "SELECT `email`,`id` from `~A` WHERE type = \"delectus_user\""
                                     bucket-name)
        results (.query bucket (N1qlQuery/simple select-expression))]
    (map #(.value %) results)))

;;; (time (delectus-users))

(defn delectus-user-ids []
  (couch-io/find-object-ids (config/delectus-users-bucket)
                            {:type (the-user-document-type)}))

;;; (time (delectus-user-ids))

(defn delectus-user-email->id [email]
  (let [found (couch-io/find-objects (config/delectus-users-bucket) {"email" email})]
    (if found
      (:document-key (first found))
      nil)))

;;; (time (delectus-user-email->id "mikel@evins.net"))
;;; (time (delectus-user-email->id "greer@evins.net"))
;;; (time (delectus-user-email->id "nobody@nowhere.net"))

(defn delectus-user-id->email [id-string]
  (let [found (couch-io/get-object (config/delectus-users-bucket) id-string)]
    (if found
      (.get found "email")
      nil)))

;;; (time (delectus-user-id->email (delectus-user-email->id "mikel@evins.net")))
;;; (time (delectus-user-id->email (delectus-user-email->id "greer@evins.net")))
;;; (time (delectus-user-id->email "NOPE!"))

(defn delectus-user-emails []
  (sort (map #(.getString % "email")
               (delectus-users))))

;;; (time (delectus-user-emails))

(defn enable-delectus-user! [email-address]
  (let [user (map->User (first (couch-io/find-objects (config/delectus-users-bucket)
                                                      {"email" email-address})))
        docid (get-id user)]
    (if user
      (.upsert (config/delectus-users-bucket)
               (to-json-document (enable user)
                                 docid))
      (throw (ex-info "No such user" {:email email-address})))))


;;; (enable-delectus-user! "mikel@evins.net")

(defn disable-delectus-user! [email-address]
  (let [user (map->User (first (couch-io/find-objects (config/delectus-users-bucket)
                                                      {"email" email-address})))
        docid (get-id user)]
    (if user
      (.upsert (config/delectus-users-bucket)
               (to-json-document (disable user)
                                 docid))
      (throw (ex-info "No such user" {:email email-address})))))


;;; (disable-delectus-user! "mikel@evins.net")

(defn add-delectus-user! [email-address & {:keys [id password-hash]
                                           :or {id (makeid)
                                                password-hash nil}}]
  (let [bucket (config/delectus-users-bucket)
        already-user-document (couch-io/get-document bucket id)]
    (if already-user-document
      (throw (ex-info "A user with the supplied ID already exists" {:id id :bucket (.name bucket)}))
      (let [new-user-map (make-user :id id
                                    :email email-address
                                    :password-hash password-hash)
            new-user-document (to-json-document new-user-map id)]
        (.insert bucket new-user-document)
        id))))

;;; (add-delectus-user! "mikel@evins.net")
;;; (enable-delectus-user! "mikel@evins.net")
;;; (add-delectus-user! "greer@evins.net")
;;; (enable-delectus-user! "greer@evins.net")
