(ns delectus-api.ensure
  (:require
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
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
                                             ~userid +type-attribute+)))
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
                                             ~collectionid +type-attribute+)))
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
                                             ~listid +type-attribute+)))
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
                                                    ~objectid +owner-attribute+)]
     (if (and found-owner#
              (= found-owner# ~userid))
       found-owner#
       (throw (ex-info "Wrong owner ID"
                       {:cause :wrong-owner
                        :expected ~userid
                        :found found-owner#})))))

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
