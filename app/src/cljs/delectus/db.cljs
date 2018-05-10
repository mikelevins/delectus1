(ns delectus.db
  (:require cljsjs.pouchdb
            cljsjs.pouchdb-find
            [delectus.utils :as utils]))

;;; ---------------------------------------------------------------------
;;; local database
;;; ---------------------------------------------------------------------

;;; creates a local (client-side) database named "delectus"
(def +local-pouch+ (js/PouchDB. "delectus"))
(.createIndex +local-pouch+
              (clj->js {:fields ["type"]}))


;;; use the PouchDB API to get and print info about the created
;;; database:
;;; (in-ns 'delectus.core)
;;; (.then (.info delectus.db/+local-pouch+) (fn [obj] (println obj)))

(defn make-list [name note]
  (clj->js
   {"_id" (str "List:" (random-uuid))
    "type" "List"
    "note" (str note)}))

(defn make-item [listid contents-map]
  (clj->js
   (merge contents-map
          {"_id" (str "ListItem:" (random-uuid))
           "type" "ListItem"
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
;;; (def $list1 (delectus.db/make-list "Test List 1" "A simple test"))
;;; (def $item1 (delectus.db/make-item (aget $list1 "_id") {"Name" "Fred"}))
;;;
;;; add the list and then the item to the local pouch
;;;
;;; (.then (.put delectus.db/+local-pouch+ $list1) #(print %))
;;; (def $listid "List:54ce8842-3f0f-4265-a234-418ef44bf37a")
;;; (.then (.put delectus.db/+local-pouch+ $item1) #(print %))
;;; (def $itemid "ListItem:3b52e1df-d560-4798-ac91-3c1aaee5d835")
;;;
;;; get the item from the pouch (use whatever id the earlier code generated to fetch it;
;;; it will differ from session to session)
;;;
;;; (.then (.get delectus.db/+local-pouch+ $itemid) #(print %))
;;;
;;; Mango query:
;;;
;;; (.find delectus.db/+local-pouch+ (clj->js {:selector {:type "List"}})  #(print %))
;;; returns:
;;; #js {:docs #js [#js {:type List, :note A simple test, :_id List:54ce8842-3f0f-4265-a234-418ef44bf37a, :_rev 1-fa4525079fda4017b35ca81e298116fd}]}

(defn show-lists []
  (.then
   (.find delectus.db/+local-pouch+
          (clj->js {:selector {:type "List"}}))
   (fn [results] (print results))))

(defn show-items []
  (.then
   (.find delectus.db/+local-pouch+
          (clj->js {:selector {:type "ListItem"}}))
   (fn [results] (print results))))

