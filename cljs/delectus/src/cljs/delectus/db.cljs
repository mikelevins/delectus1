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
;;; (.then (.info +local-pouch+) (fn [obj] (println obj)))

(defn make-db-list [name note]
  (clj->js
   {"_id" (str "List:" (random-uuid))
    "_note" (str note)}))

(defn make-db-item [listid contents-map]
  (clj->js
   (merge contents-map
          {"_id" (str "ListItem:" (random-uuid))
           "_list" (str listid)})))

