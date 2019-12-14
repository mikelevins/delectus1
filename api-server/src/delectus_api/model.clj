(ns delectus-api.model
  (:require
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.couchio :as couchio]))

;;; ---------------------------------------------------------------------
;;; User, Collection, and List
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

;;; (make-user-document :email "mikel@evis.net")

(defn make-collection-document [& {:keys [id name owner-id lists deleted]
                                   :or {id (makeid)
                                        name nil
                                        owner-id nil
                                        ;; lists is a set of list-ids
                                        ;; we represent that as a JSON object
                                        ;; the keys are the lists, the vals are ignored
                                        lists {}
                                        deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-collection-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-collection-document"})
  (let [obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ lists
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (make-collection-document :name "Random stuff" :owner-id $mikelid)

(defn make-default-collection [& {:keys [id owner-id]
                                  :or {id (makeid)
                                       owner-id nil}}]
  (let [obj-id (makeid)
        obj-map {+type-attribute+ +collection-type+
                 +id-attribute+ obj-id
                 +name-attribute+ +standard-default-collection-name+
                 +owner-id-attribute+ owner-id
                 +lists-attribute+ {}
                 +deleted-attribute+ false}]
    (couchio/make-json-document obj-id obj-map)))

(defn make-list-document [& {:keys [id name owner-id columns items deleted]
                             :or {id (makeid)
                                  name nil
                                  owner-id nil
                                  columns {}
                                  items {}
                                  deleted false}}]
  (errors/error-if-nil name "Missing name parameter" {:context "make-list-document"})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context "make-list-document"})
  (let [obj-map {+type-attribute+ +list-type+
                 +id-attribute+ id
                 +name-attribute+ name
                 +owner-id-attribute+ owner-id
                 +columns-attribute+ columns
                 +items-attribute+ items
                 +deleted-attribute+ deleted}]
    (couchio/make-json-document id obj-map)))

