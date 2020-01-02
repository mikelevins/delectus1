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
   [delectus-api.couchio :as couchio]
   [delectus-api.utilities :as utils])
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
  (let [obj-map {+type-key+ +user-type+
                 +id-key+ id
                 +email-key+ email
                 +name-key+ name
                 +password-hash-key+ password-hash
                 +enabled-key+ enabled}]
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
                                        userid +type-key+))))

(defn get-user [userid]
  (couchio/get-object-of-type (config/delectus-users-bucket) userid +user-type+))

;;; finding registered users
;;; ---------------------------------------------------------------------

(defn email->user [email]
  (let [found (couchio/find-objects (config/delectus-users-bucket)
                                    :keys []
                                    :match {+type-key+ +user-type+
                                            +email-key+ email})]
    (if (empty? found)
      nil
      (first found))))

(defn email->userid [email]
  (let [found (couchio/find-objects (config/delectus-users-bucket)
                                    :keys []
                                    :match {+type-key+ +user-type+
                                            +email-key+ email})]
    (if (empty? found)
      nil
      (.get (first found) +id-key+))))

;;; (email->user "mikel@evins.net")
;;; (email->userid "mikel@evins.net")

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
  (let [obj-map {+type-key+ +collection-type+
                 +id-key+ id
                 +name-key+ name
                 +owner-key+ owner
                 +deleted-key+ deleted}]
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

(defn count-collections [userid]
  (couchio/with-couchbase-exceptions-rethrown
    (let [selector (str "SELECT COUNT(*) AS `collectioncount` FROM `delectus_content` "
                        "WHERE `type` = '" +collection-type+ "' AND `owner` = '" userid "';")
          results (.query (config/delectus-content-bucket) (N1qlQuery/simple selector))]
      (.get (.value (.get (.allRows results) 0)) "collectioncount"))))

;;; (time (count-collections $mikelid))

(defn collection-exists? [collectionid]
  (and (couchio/id-exists? (config/delectus-content-bucket) collectionid)
       (= +collection-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +type-key+))))

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
  (let [obj-map {+type-key+ +list-type+
                 +id-key+ id
                 +name-key+ name
                 +owner-key+ owner
                 +collection-key+ collection
                 +columns-key+ columns
                 +deleted-key+ deleted}]
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

(defn count-lists [userid collectionid]
  (if (nil? collectionid)
    ;;; count uncollected lists
    (couchio/with-couchbase-exceptions-rethrown
      (let [selector (str "SELECT COUNT(*) AS `listcount` FROM `delectus_content` "
                          "WHERE `collection` IS NULL AND `owner` = '" userid "';")
            results (.query (config/delectus-content-bucket) (N1qlQuery/simple selector))]
        (.get (.value (.get (.allRows results) 0)) "listcount")))
    ;;; count lists in collectionid
    (couchio/with-couchbase-exceptions-rethrown
      (let [selector (str "SELECT COUNT(*) AS `listcount` FROM `delectus_content` "
                          "WHERE `collection` = '" collectionid "' AND `owner` = '" userid "';")
            results (.query (config/delectus-content-bucket) (N1qlQuery/simple selector))]
        (.get (.value (.get (.allRows results) 0)) "listcount")))))

;;; (time (count-lists $mikelid nil))

(defn list-exists? [listid]
  (and (couchio/id-exists? (config/delectus-content-bucket) listid)
       (= +list-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        listid +type-key+))))

(defn list-name-exists? [userid listname]
  (let [found (couchio/find-objects (config/delectus-content-bucket)
                                    :keys ["id"]
                                    :match {+type-key+ +list-type+
                                            +name-key+ listname
                                            +owner-key+ userid})]
    (if (empty? found)
      false
      true)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (list-name-exists? $mikelid "Zipcodes")

(defn get-list [listid]
  (couchio/get-object-of-type (config/delectus-content-bucket) listid +list-type+))


;;; list columns
;;; ---------------------------------------------------------------------

(defn make-column [& {:keys [id name deleted]
                      :or {id nil
                           name nil
                           deleted false}}]
  (errors/error-if-nil id "Missing id parameter" {:context 'make-column})
  (errors/error-if-nil name "Missing name parameter" {:context 'make-column})
  (let [col-map {+id-key+ id
                 +name-key+ name
                 +deleted-key+ deleted}]
    (couchio/make-json-object col-map)))

;;; (make-column :id "0" :name "Foo" :deleted false)

(defn column-attribute-values [listid attribute-name]
  (let [found-list (ensure/ensure-list listid)
        cols (.get found-list +columns-key+)
        ids (.getNames cols)]
    (map (fn [id] (.get (.get cols id) attribute-name))
         ids)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (column-attribute-values $listid "id"))

(defn column-ids [listid]
  (column-attribute-values listid +id-key+))

(defn get-list-columns [listid]
  (let [found-list (ensure/ensure-list listid)]
    (.get found-list +columns-key+)))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (get-list-columns $listid)

(defn get-list-column [listid columnid]
  (ensure/ensure-list-exists listid)
  (couchio/get-document-path (config/delectus-content-bucket)
                             listid (str +columns-key+ "." columnid)))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (get-list-column $listid "8")

(defn get-list-column-ids [listid]
  (let [found-list (ensure/ensure-list listid)]
    (.getNames (.get found-list +columns-key+))))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (get-list-column-ids $listid)

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (column-ids $listid))

(defn columnid-exists? [listid columnid]
  (let [keypath (str +columns-key+ "." columnid)]
    (couchio/document-path-exists? (config/delectus-content-bucket)
                                   listid
                                   keypath)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (column-id-exists? $listid "16"))

(defn get-column [listid columnid]
  (let [keypath (str +columns-key+ "." columnid)]
    (if (couchio/document-path-exists? (config/delectus-content-bucket)
                                       listid
                                       keypath)
      (couchio/get-document-path (config/delectus-content-bucket)
                                 listid
                                 keypath)
      nil)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (get-column $listid "22"))

(defn next-column-id [listid]
  (if (nil? (get-list-columns listid))
    "0"
    (let [ids (column-attribute-values listid +id-key+)]
      (if (empty? ids)
        "0"
        (let [ids (sort < (map #(Integer. %) ids))
            maxid (apply max ids)]
        (str (+ 1 maxid)))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (next-column-id $listid))

(defn column-names [listid]
  (column-attribute-values listid +name-key+))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (column-names $listid))

(defn column-name-exists? [listid name]
  (let [cols (get-list-columns listid)
        ids (.getNames cols)]
    (some (fn [id] (= name (.get (.get cols id) +name-key+)))
          ids)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (column-name-exists? $listid "dst"))

(defn column-deleted? [listid columnid]
  (let [found-column (get-list-column listid columnid)]
    (errors/error-if-not found-column "Column ID not found" {:context 'column-deleted? :id columnid})
    (.get found-column +deleted-key+)))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (column-deleted? $listid "8")

(defn mark-column-deleted! [listid columnid deleted?]
  (couchio/update-document-path! (config/delectus-content-bucket)
                                 listid (str +columns-key+ "." columnid "." +deleted-key+)
                                 deleted?))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (mark-column-deleted! $listid "8" true)
;;; (mark-column-deleted! $listid "8" false)
;;; (column-deleted? $listid "8")

(defn update-column-name! [listid columnid new-name]
  (couchio/update-document-path! (config/delectus-content-bucket)
                                 listid (str +columns-key+ "." columnid "." +name-key+)
                                 new-name))

;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (update-column-name! $listid "8" "Circa")
;;; (update-column-name! $listid "8" "Year")

;;; assert-column!'s required behavior is complex:
;;; Case 1: columnid and column-name exist on the same column:
;;;         the requested column already exists; just make sure it
;;;         isn't marked deleted
;;; Case 2: columnid and column-name exist on different columns:
;;;         signal an error; we're trying to rename a column using a
;;;         duplicate name
;;; Case 3: columnid exists, column-name doesn't:
;;;         rename the identified column to column-name
;;; Case 4: column-name exists, columnid doesn't:
;;;         signal an error; we're trying to use a duplicate name
;;; Case 5: columnid and column name don't exist in the list: 
;;;         add the requested column

(defn assert-column! [listid columnid column-name]
  (let [found-list (ensure/ensure-list listid)]
    (errors/error-if-nil found-list "No such list"
                         {:context assert-column! :id listid})
    ;; ensure that the list has a columns object
    (let [cols (get-list-columns listid)]
      (when (nil? cols)
        (couchio/upsert-document-path! (config/delectus-content-bucket)
                                       listid +columns-key+ (couchio/make-json-object {}))))
    ;; handle the request to create a column
    (let [found-columnid? (columnid-exists? listid columnid)
          found-column-name? (column-name-exists? listid column-name)]
      ;; handle cases
      (cond
        ;; columnid and column-name both exist
        (and found-columnid? found-column-name?)
        (let [found-column (get-list-column listid columnid)
              found-name (.get found-column +name-key+)]
          (if (= column-name found-name)
            ;; they exist on the same column; just mark it undeleted
            (mark-column-deleted! listid columnid false)
            ;; they exist on different columns; signal an error
            (throw (ex-info (str "A different column is using the name \"" column-name "\"")
                            {:context assert-column! :name column-name}))))
        ;; columnid exists, column-name doesn't
        (and found-columnid? (not found-column-name?))
        (update-column-name! listid columnid column-name)
        ;; column-name exists, columnid doesn't
        (and found-column-name? (not found-columnid?))
        (throw (ex-info (str "Another column is using the name " column-name)
                        {:context assert-column! :listid listid
                         :name column-name}))
        ;; neither columnid nor column-name exists; add the column
        (and (not found-column-name?) (not found-columnid?))
        (let [column-obj (make-column :id columnid :name column-name :deleted false)
              column-keypath (str +columns-key+ "." columnid)]
          (couchio/with-couchbase-exceptions-rethrown
            (couchio/upsert-document-path! (config/delectus-content-bucket)
                                           listid column-keypath column-obj)))
        ;; something impossible has happened
        :else (throw (ex-info (str "Inconsistent results checking for columnid and column-name")
                              {:context assert-column! :listid listid
                               :columnid columnid :column-name column-name}))))))

;;; (def $listid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (assert-column! $listid (next-column-id $listid) "Genre")
;;; (time (column-names $listid))


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
  (let [obj-map {+type-key+ +item-type+
                 +id-key+ id
                 +owner-key+ owner
                 +list-key+ list
                 +fields-key+ fields
                 +deleted-key+ deleted}]
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

(defn count-items [userid listid]
  (couchio/with-couchbase-exceptions-rethrown
    (let [selector (str "SELECT COUNT(*) FROM `delectus_content` "
                        "WHERE `list` = '" listid "'"
                        " AND `owner` = '" userid "'"
                        " AND `type` = '" +item-type+ "' ;")
          results (.query (config/delectus-content-bucket) (N1qlQuery/simple selector))]
      (.get (.value (.get (.allRows results) 0)) "$1"))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (def $zipcodesid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (time (count-items $mikelid $moviesid))
;;; (time (count-items $mikelid $zipcodesid))

(defn get-items [userid listid fields]
  (couchio/with-couchbase-exceptions-rethrown
    (couchio/find-objects (config/delectus-content-bucket)
                          :keys fields
                          :match {+owner-key+ userid
                                  +list-key+ listid
                                  +type-key+ +item-type+})))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (def $zipcodesid "3518c607-a3cb-4cd9-b21f-05845827ca0d")
;;; (def $items (time (get-items $mikelid $moviesid [])))
;;; (.toMap (nth $items 0))
;;; (time (get-items $mikelid $zipcodesid))

(defn item-exists? [itemid]
  (and (couchio/id-exists? (config/delectus-content-bucket) itemid)
       (= +item-type+
          (couchio/get-object-attribute (config/delectus-content-bucket)
                                        itemid +type-key+))))

(defn get-item [itemid]
  (couchio/get-object-of-type (config/delectus-content-bucket) itemid +item-type+))

