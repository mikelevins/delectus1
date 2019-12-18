(ns delectus-api.handlers
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.auth :as auth]
   [delectus-api.api :as api]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.model :as model]
   [delectus-api.schema :as schema]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   )
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; error handlers
;;; ---------------------------------------------------------------------

(def +exception-handlers+
  {
   :authentication-failed unauthorized
   :collection-name-exists conflict
   :collection-not-found not-found
   :column-name-exists conflict
   :couchbase-exception internal-server-error
   :exception internal-server-error
   :list-name-exists conflict
   :list-not-found not-found
   :login-failed unauthorized
   :user-not-found not-found
   :wrong-document-type internal-server-error
   :wrong-object-type internal-server-error
   })

(defn handle-exception [ex]
  (let [msg (.getMessage ex)
        data (ex-data ex)
        cause (:cause data)
        handler (get +exception-handlers+ cause internal-server-error)]
    (let [info (str msg ". " data)]
      (handler info))))

;;; ---------------------------------------------------------------------
;;; error-handling macros
;;; ---------------------------------------------------------------------

(defmacro with-errors-handled [& forms]
  (let [exname (gensym)]
    `(try
       (do ~@forms)
       (catch clojure.lang.ExceptionInfo ~exname
         (handle-exception ~exname)))))

;;; ---------------------------------------------------------------------
;;; endpoint handlers
;;; ---------------------------------------------------------------------

;;; /api/user

(defn authenticate [userid password]
  (with-errors-handled
    (ok {:token (auth/make-auth-token (api/authenticate userid password))})))

(defn login [email password]
  (with-errors-handled
    (ok {:token (auth/make-auth-token (api/login email password))})))

(defn userid [email]
  (with-errors-handled
    (ok (api/userid email))))

(defn userdata [userid]
  (with-errors-handled
    (ok (api/userdata userid))))

;;; /api/collection

(defn collections [userid]
  (with-errors-handled
    (ok (api/collections userid))))

(defn collection-with-id [userid collectionid]
  (with-errors-handled
    (ok (api/collection-with-id userid collectionid))))

(defn collection-name [userid collectionid]
  (with-errors-handled
    (ok (api/collection-name userid collectionid))))

(defn find-collection-with-name [userid name]
  (with-errors-handled
    (ok (api/find-collection-with-name userid name))))

(defn rename-collection [userid collectionid newname]
  (with-errors-handled
    (ok (api/rename-collection userid collectionid newname))))

(defn new-collection [userid name]
  (with-errors-handled
    (ok (api/new-collection userid name))))

(defn delete-collection [userid collectionid]
  (with-errors-handled
    (ok (api/delete-collection userid collectionid))))

(defn undelete-collection [userid collectionid]
  (with-errors-handled
    (ok (api/undelete-collection userid collectionid))))

(defn collection-deleted? [userid collectionid]
  (with-errors-handled
    (ok (api/collection-deleted? userid collectionid))))

(defn collection-lists [userid collectionid]
  (with-errors-handled
    (ok (api/collection-lists userid collectionid))))

;;; /api/list

(defn lists [userid]
  (with-errors-handled
    (ok (api/lists userid))))

(defn move-list-to-collection [userid listid collectionid]
  (with-errors-handled
    (ok (api/move-list-to-collection userid listid collectionid))))

(defn make-list-uncollected [userid listid]
  (with-errors-handled
    (ok (api/make-list-uncollected userid listid))))

(defn list-with-id [userid listid]
  (with-errors-handled
    (ok (api/list-with-id userid listid))))

(defn list-name [userid listid]
  (with-errors-handled
    (ok (api/list-name userid listid))))

(defn find-list-with-name [userid name]
  (with-errors-handled
    (ok (api/find-list-with-name userid name))))

(defn rename-list [userid listid newname]
  (with-errors-handled
    (ok (api/rename-list userid listid newname))))

(defn new-list [userid name]
  (with-errors-handled
    (ok (api/new-list userid name))))

(defn delete-list [userid listid]
  (with-errors-handled
    (ok (api/delete-list userid listid))))

(defn undelete-list [userid listid]
  (with-errors-handled
    (ok (api/undelete-list userid listid))))

(defn list-deleted? [userid listid]
  (with-errors-handled
    (ok (api/list-deleted? userid listid))))

(defn list-columns [userid listid]
  (with-errors-handled
    (ok (api/list-columns userid listid))))

(defn new-column [userid listid name]
  (with-errors-handled
    (ok (api/new-column userid listid name))))
