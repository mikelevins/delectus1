(ns delectus.ensure
  (:require
   [delectus.configuration :as config]
   [delectus.constants :refer :all]
   [delectus.couchio :as couchio]
   )
  (:import
   (com.couchbase.client.java.document JsonDocument)))

;;; ---------------------------------------------------------------------
;;; ensuring object type and attribute invariants
;;; ---------------------------------------------------------------------

(defmacro ensure-user-exists [userid]
  `(if (and (couchio/id-exists? (config/delectus-users-bucket) ~userid)
            (= +user-type+
               (couchio/get-object-attribute (config/delectus-users-bucket)
                                             ~userid +type-key+)))
     ~userid
     (throw (ex-info "No such user"
                     {:cause :user-not-found
                      :userid ~userid}))))

(defmacro ensure-user [userid]
  `(let [found-user# (couchio/get-object-of-type (config/delectus-users-bucket)
                                                 ~userid
                                                 +user-type+)]
     (or found-user#
         (throw (ex-info "No such user"
                         {:cause :user-not-found
                          :userid ~userid})))))

(defmacro ensure-collection-exists [collectionid]
  `(if (and (couchio/id-exists? (config/delectus-content-bucket) ~collectionid)
            (= +collection-type+
               (couchio/get-object-attribute (config/delectus-content-bucket)
                                             ~collectionid +type-key+)))
     ~collectionid
     (throw (ex-info "No such collection"
                     {:cause :collection-not-found
                      :collectionid ~collectionid}))))

(defmacro ensure-collection [collectionid]
  `(let [found-collection# (couchio/get-object-of-type (config/delectus-content-bucket)
                                                       ~collectionid
                                                       +collection-type+)]
     (or found-collection#
         (throw (ex-info "No such collection"
                         {:cause :collection-not-found
                          :collectionid ~collectionid})))))

(defmacro ensure-list-exists [listid]
  `(if (and (couchio/id-exists? (config/delectus-content-bucket) ~listid)
            (= +list-type+
               (couchio/get-object-attribute (config/delectus-content-bucket)
                                             ~listid +type-key+)))
     ~listid
     (throw (ex-info "No such list"
                     {:cause :list-not-found
                      :listid ~listid}))))

(defmacro ensure-list [listid]
  `(let [found-list# (couchio/get-object-of-type (config/delectus-content-bucket)
                                                 ~listid
                                                 +list-type+)]
     (or found-list#
         (throw (ex-info "No such list"
                         {:cause :list-not-found
                          :listid ~listid})))))

(defmacro ensure-owner [objectid userid]
  `(let [found-owner# (couchio/get-object-attribute (config/delectus-content-bucket)
                                                    ~objectid +owner-key+)]
     (if (and found-owner#
              (= found-owner# ~userid))
       found-owner#
       (throw (ex-info "Wrong owner ID"
                       {:cause :wrong-owner
                        :expected ~userid
                        :found found-owner#})))))


(defmacro ensure-item-exists [itemid]
  `(if (and (couchio/id-exists? (config/delectus-content-bucket) ~itemid)
            (= +item-type+
               (couchio/get-object-attribute (config/delectus-content-bucket)
                                             ~itemid +type-key+)))
     ~itemid
     (throw (ex-info "No such item"
                     {:cause :item-not-found
                      :itemid ~itemid}))))

;;; (ensure-item-exists "00007c0b-21a7-426f-8b76-8440d75f6ac3")
;;; (ensure-item-exists "NO")

(defmacro ensure-item [itemid]
  `(let [found-item# (couchio/get-document-of-type (config/delectus-content-bucket)
                                                   ~itemid
                                                   +item-type+)]
     (or found-item#
         (throw (ex-info "No such item"
                         {:cause :item-not-found
                          :itemid ~itemid})))))

;;; (ensure-item "00007c0b-21a7-426f-8b76-8440d75f6ac3")
;;; (ensure-item "002153f5-431a-47a3-82d5-f2161ed1d4d0")
;;; (ensure-item "NO")

;;; ---------------------------------------------------------------------
;;; model type-checks
;;; ---------------------------------------------------------------------

(defmacro ensure-json-object [object]
  `(if (instance? JsonObject ~object)
     ~object
     (throw (ex-info "Not a JSON object"
                     {:cause :wrong-object-type
                      :expected-type JsonObject
                      :found-type (type ~object)}))))

(defmacro ensure-document-type [json-doc type-string]
  `(if (instance? JsonDocument ~json-doc)
     (let [found-type# (.get (.content ~json-doc) +type-key+)]
       (or (and (= ~type-string found-type#))
           (throw (ex-info "Wrong document type"
                           {:cause :wrong-document-type
                            :expected-type ~type-string
                            :found-type found-type#}))))
     (throw (ex-info "Wrong object type"
                     {:cause :wrong-object-type
                      :expected-type JsonDocument
                      :found-type (type ~json-doc)}))))
