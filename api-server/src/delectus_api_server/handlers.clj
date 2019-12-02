(ns delectus-api-server.handlers
  (:require
   [buddy.hashers :as hashers]
   [clojure.data.json :as json]
   [clojure.pprint :as pp]
   [delectus-api-server.api :as api]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.model :as model]
   [delectus-api-server.utilities :refer [fmt]]
   [hiccup.core :refer :all]
   [org.httpkit.server :as server]
   [ring.util.codec :refer [url-decode]])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))


;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

(defn echo [req]
  (let [req-keys (keys req)
        req-vals (map #(if (instance? org.httpkit.server.AsyncChannel %)
                         (.toString %)
                         %)
                      (vals req))]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/write-str (zipmap req-keys req-vals))}))

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [mgr (.clusterManager (config/couchbase-cluster)
                                    (:delectus-admin-user (config/delectus-configuration))
                                    (:delectus-admin-password (config/delectus-configuration)))
               info (.raw (.info mgr))]
           (.toString info))})

;;; ---------------------------------------------------------------------
;;; user handlers
;;; ---------------------------------------------------------------------

(defn login [req]
  (let [params (:params req)
        supplied-email (:email params)
        supplied-password (:password params)
        found-user (api/login supplied-email supplied-password)]
    (if found-user
      (let [usermap (into {} (.toMap found-user))]
        {:status  200
         :headers {"Content-Type" "application/json"}
         :body (json/write-str (merge usermap {:token "testing login"}))})
      {:status  401
       :headers {"Content-Type" "application/json"}
       :body    (json/write-str {:message "Login failed"})})))

(defn userid [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params request))]
              (json/write-str (api/userid email)))})


;;; ---------------------------------------------------------------------
;;; collections handlers
;;; ---------------------------------------------------------------------

(defn collections [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str
               (map #(json/read-str (.toString %))
                    (api/collections (api/userid email)))))})

(defn collection-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-id (:id (:params req))
                  collection (api/collection-with-id (api/userid email) collection-id)]
              (if collection
                (.toString collection)
                (json/write-str nil)))})

(defn collection-name [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-id (:id (:params req))
                  name (api/collection-name (api/userid email) collection-id)]
              (if name
                (json/write-str name)
                (json/write-str nil)))})

(defn collection-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-name (:name (:params req))
                  collection (api/collection-named (api/userid email) collection-name)]
              (if collection
                (.toString collection)
                (json/write-str nil)))})

(defn rename-collection [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  new-name (:newname (:params req))
                  result (api/rename-collection userid collection-id new-name)]
              (json/write-str result))})

(defn new-collection [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  name (:name (:params req))]
              (json/write-str
               (api/new-collection :id (makeid)
                                   :name name
                                   :owner-id (api/userid email))))})

(defn delete-collection [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  result (api/mark-collection-deleted userid collection-id true)]
              (json/write-str result))})

(defn undelete-collection [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  result (api/mark-collection-deleted userid collection-id false)]
              (json/write-str result))})

(defn collection-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  result (api/collection-deleted? userid collection-id)]
              (json/write-str result))})

(defn collection-lists [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-id (:id (:params req))
                  lists (api/collection-lists (api/userid email) collection-id)]
              (if (nil? lists)
                (json/write-str nil)
                (json/write-str lists)))})

(defn collection-add-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  list-id (:listid (:params req))
                  result (api/collection-add-list userid collection-id list-id)]
              (json/write-str result))})

(defn collection-remove-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  collection-id (:collectionid (:params req))
                  list-id (:listid (:params req))
                  result (api/collection-remove-list userid collection-id list-id)]
              (json/write-str result))})

;;; ---------------------------------------------------------------------
;;; list handlers
;;; ---------------------------------------------------------------------

(defn lists [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str
               (map #(json/read-str (.toString %))
                    (api/lists (api/userid email)))))})

(defn list-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-id (:listid (:params req))
                  found-list (api/list-with-id (api/userid email) list-id)]
              (if found-list
                (.toString found-list)
                (json/write-str nil)))})

(defn list-name [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-name userid list-id)]
              (json/write-str result))})

(defn list-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-name (:name (:params req))
                  found-list (api/list-named (api/userid email) list-name)]
              (if found-list
                (.toString found-list)
                (json/write-str nil)))})

(defn rename-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  new-name (:newname (:params req))
                  result (api/rename-list userid list-id new-name)]
              (json/write-str result))})

(defn new-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  name (:name (:params req))]
              (json/write-str
               (api/new-list :id (makeid)
                             :name name
                             :owner-id (api/userid email))))})

(defn delete-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/mark-list-deleted userid list-id true)]
              (json/write-str result))})

(defn undelete-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/mark-list-deleted userid list-id false)]
              (json/write-str result))})

(defn list-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-deleted? userid list-id)]
              (json/write-str result))})

(defn list-columns [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-columns userid list-id)]
              (.toString result))})

(defn new-column [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-name (:columnname (:params req))
                  result (api/new-column :owner-id userid :list-id list-id :name column-name)]
              (.toString result))})

(defn column-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  result (api/column-with-id :owner-id userid :list-id list-id :column-id column-id)]
              (if result
                (.toString result)
                (json/write-str nil)))})

(defn column-name [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  result (api/column-name :owner-id userid :list-id list-id :column-id column-id)]
              (if result
                (json/write-str result)
                (json/write-str nil)))})

(defn column-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-name (:columnname (:params req))
                  result (api/column-named :owner-id userid :list-id list-id :column-name column-name)]
              (if result
                (.toString result)
                (json/write-str nil)))})

;;; returns true if deleted, false if not deleted, nil if there is no such column
(defn column-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  result (api/column-deleted? :owner-id userid :list-id list-id :column-id column-id)]
              (json/write-str result))})

(defn delete-column [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  result (api/mark-column-deleted userid list-id column-id true)]
              (json/write-str result))})

(defn undelete-column [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  result (api/mark-column-deleted userid list-id column-id false)]
              (json/write-str result))})

(defn rename-column [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  column-id (:columnid (:params req))
                  column-name (:columnname (:params req))
                  result (api/rename-column userid list-id column-id column-name)]
              (json/write-str result))})

(defn list-items [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-items userid list-id)]
              (.toString result))})

(defn item-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  result (api/item-with-id userid list-id item-id)]
              (if (nil? result)
                (json/write-str nil)
                (.toString result)))})

(defn new-item [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  result (api/new-item :list-id list-id :owner-id userid)]
              (.toString result))})

(defn delete-item [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  result (api/mark-item-deleted userid list-id item-id true)]
              (.toString result))})

(defn undelete-item [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  result (api/mark-item-deleted userid list-id item-id false)]
              (.toString result))})

(defn item-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  result (api/item-deleted? userid list-id item-id)]
              (if (nil? result)
                (json/write-str nil)
                (.toString result)))})

(defn item-column-value [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  column-id (:columnid (:params req))
                  result (api/item-column-value userid list-id item-id column-id)]
              (json/write-str result))})

(defn set-item-column-value [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (model/email->userid email)
                  list-id (:listid (:params req))
                  item-id (:itemid (:params req))
                  column-id (:columnid (:params req))
                  item-value (:newvalue (:params req))
                  result (api/set-item-column-value userid list-id item-id column-id item-value)]
              (if (nil? result)
                (json/write-str nil)
                (json/write-str result)))})