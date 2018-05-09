(ns delectus.db
  (:require cljsjs.pouchdb
            [delectus.utils :as utils]))

;;; ---------------------------------------------------------------------
;;; local database
;;; ---------------------------------------------------------------------

;;; creates a local (client-side) database named "delectus"
(def +local-pouch+ (js/PouchDB. "delectus"))

;;; use the PouchDB API to get and print info about the created
;;; database:
;;; (in-ns 'delectus.core)
;;; (.then (.info db/+local-pouch+) (fn [obj] (println obj)))

(defn make-db-list [name note]
  (clj->js
   {"_id" (str "List:" (random-uuid))
    "note" (str note)}))

(defn make-db-item [listid contents-map]
  (clj->js
   (merge contents-map
          {"_id" (str "ListItem:" (random-uuid))
           "list" (str listid)})))

;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------
;;; currently working:
;;;
;;; (in-ns 'delectus.core)
;;;
;;; create a list and add an item to it
;;;
;;; (def $list1 (db/make-db-list "Test List 1" "A simple test"))
;;; (def $item1 (db/make-db-item (aget $list1 "_id") {"Name" "Fred"}))
;;;
;;; add the list and then the item to the local pouch
;;;
;;; (.then (.put db/+local-pouch+ $list1) #(print %))
;;; (.then (.put db/+local-pouch+ $item1) #(print %))
;;;
;;; get the item from the pouch (use whatever id the earlier code generated to fetch it;
;;; it will differ from session to session)
;;;
;;; (.then (.get db/+local-pouch+ "ListItem:0a72b015-d5e8-408b-91ee-c2fd96eb6bf0") #(print %))

