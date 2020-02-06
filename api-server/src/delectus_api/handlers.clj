(ns delectus-api.handlers
  (:require
   [clj-time.core :as t]
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
   [hiccup.core :refer [html h]]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
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
   :column-not-found not-found
   :couchbase-exception internal-server-error
   :exception internal-server-error
   :item-not-found not-found
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
    (ok {:userid userid
         :token (auth/make-auth-token (api/authenticate userid password))})))

(defn get-login []
  (header (ok (html [:p "Login form here"]))
          "Content-Type" "text/html"))

(defn login [email password]
  (with-errors-handled
    (let [userid (api/userid email)]
      (ok {:email email :userid userid
           :token (auth/make-auth-token (api/login email password))}))))

(defn logout [token]
  (with-errors-handled
    (ok "Logged out")))

(defn userid [email]
  (with-errors-handled
    (ok (api/userid email))))

(defn userdata [userid]
  (with-errors-handled
    (ok (api/userdata userid))))

;;; /api/collection

(defn collections [userid fields]
  (with-errors-handled
    (ok (api/collections userid fields))))

(defn collection-with-id [userid collectionid fields]
  (with-errors-handled
    (ok (api/collection-with-id userid collectionid fields))))

(defn collection-name [userid collectionid]
  (with-errors-handled
    (ok (api/collection-name userid collectionid))))

(defn find-collections-with-name [userid name fields]
  (with-errors-handled
    (ok (api/find-collections-with-name userid name fields))))

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

(defn collection-lists [userid collectionid fields]
  (with-errors-handled
    (ok (api/collection-lists userid collectionid fields))))

;;; /api/list

(defn lists [userid fields]
  (with-errors-handled
    (ok (api/lists userid fields))))

(defn move-list-to-collection [userid listid collectionid]
  (with-errors-handled
    (ok (api/move-list-to-collection userid listid collectionid))))

(defn make-list-uncollected [userid listid]
  (with-errors-handled
    (ok (api/make-list-uncollected userid listid))))

(defn list-with-id [userid listid fields]
  (with-errors-handled
    (ok (api/list-with-id userid listid fields))))

(defn list-name [userid listid]
  (with-errors-handled
    (ok (api/list-name userid listid))))

(defn find-lists-with-name [userid name]
  (with-errors-handled
    (ok (api/find-lists-with-name userid name))))

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

(defn column-with-id [userid listid columnid]
  (with-errors-handled
    (ok (api/column-with-id userid listid columnid))))

(defn column-name [userid listid columnid]
  (with-errors-handled
    (ok (api/column-name userid listid columnid))))

(defn column-named [userid listid name]
  (with-errors-handled
    (ok (api/column-named userid listid name))))

(defn column-deleted [userid listid columnid]
  (with-errors-handled
    (ok (api/column-deleted userid listid columnid))))

(defn delete-column [userid listid columnid]
  (with-errors-handled
    (ok (api/delete-column userid listid columnid))))

(defn undelete-column [userid listid columnid]
  (with-errors-handled
    (ok (api/undelete-column userid listid columnid))))

(defn rename-column [userid listid columnid name]
  (with-errors-handled
    (ok (api/rename-column userid listid columnid name))))

(defn list-items [userid listid & {:keys [offset limit]
                                   :or {offset 0 limit 100}}]
  (with-errors-handled
    (ok (api/list-items userid listid :offset offset :limit limit))))

(defn list-item-with-id [userid listid itemid]
  (with-errors-handled
    (ok (api/list-item-with-id userid listid itemid))))

(defn new-list-item [userid listid]
  (with-errors-handled
    (ok (api/new-list-item userid listid))))

(defn delete-list-item [userid listid itemid]
  (with-errors-handled
    (ok (api/delete-list-item userid listid itemid))))

(defn undelete-list-item [userid listid itemid]
  (with-errors-handled
    (ok (api/undelete-list-item userid listid itemid))))

(defn list-item-deleted [userid listid itemid]
  (with-errors-handled
    (ok (api/list-item-deleted userid listid itemid))))

(defn item-column-value [userid listid itemid columnid]
  (with-errors-handled
    (ok (api/item-column-value userid listid itemid columnid))))

(defn set-item-column-value! [userid listid itemid columnid value]
  (with-errors-handled
    (ok (api/set-item-column-value! userid listid itemid columnid value))))
