(ns delectus-api-server.handlers
  (:require
   [buddy.hashers :as hashers]
   [clojure.data.json :as json]
   [delectus-api-server.api :as api]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.utilities :refer [fmt]]
   [hiccup.core :refer :all]
   [org.httpkit.server :as server])
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
;;; support functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; user handlers
;;; ---------------------------------------------------------------------

(defn login [req]
  (let [params (:params req)
        supplied-email (:email params)
        supplied-password (:password params)
        found-user (api/login supplied-email supplied-password)]
    (if found-user
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body (json/write-str {:token "testing login"})}
      {:status  401
       :headers {"Content-Type" "text/html"}
       :body    (html [:h1 "Delectus 2"]
                      [:p "Login failed"])})))

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
                  userid (api/email->userid email)
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
                  userid (api/email->userid email)
                  collection-id (:collectionid (:params req))
                  result (api/mark-collection-deleted userid collection-id true)]
              (json/write-str result))})

(defn undelete-collection [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
                  collection-id (:collectionid (:params req))
                  result (api/mark-collection-deleted userid collection-id false)]
              (json/write-str result))})

(defn collection-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
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
                  userid (api/email->userid email)
                  collection-id (:collectionid (:params req))
                  list-id (:listid (:params req))
                  result (api/collection-add-list userid collection-id list-id)]
              (json/write-str result))})

(defn collection-remove-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
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
                  list-id (:id (:params req))
                  found-list (api/list-with-id (api/userid email) list-id)]
              (if found-list
                (.toString found-list)
                (json/write-str nil)))})

(defn list-name [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
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
                  userid (api/email->userid email)
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
                  userid (api/email->userid email)
                  list-id (:listid (:params req))
                  result (api/mark-list-deleted userid list-id true)]
              (json/write-str result))})

(defn undelete-list [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
                  list-id (:listid (:params req))
                  result (api/mark-list-deleted userid list-id false)]
              (json/write-str result))})

(defn list-deleted? [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-deleted? userid list-id)]
              (json/write-str result))})

(defn list-columns [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
                  list-id (:listid (:params req))
                  result (api/list-columns userid list-id)]
              (.toString result))})

(defn new-column [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  userid (api/email->userid email)
                  list-id (:listid (:params req))
                  column-name (:columnname (:params req))
                  result (api/new-column :owner-id userid :list-id list-id :name column-name)]
              (.toString result))})
