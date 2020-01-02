(ns delectus-api.datatools
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [delectus-api.configuration :as config]
            [delectus-api.constants :refer :all]
            [delectus-api.couchio :as couchio]
            [delectus-api.ensure :refer :all]
            [delectus-api.errors :refer :all]
            [delectus-api.identifiers :refer [makeid]]
            [delectus-api.model :as model]))

(defn canonicalize-csv-field [field]
  (if (= "#f" field)
    false
    field))

(defn canonicalize-csv-row [row]
  (map canonicalize-csv-field row))

(defn read-csv-file [filename]
  (with-open [reader (io/reader filename)]
    (map canonicalize-csv-row
         (mapv identity (csv/read-csv reader)))))

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (count $data)
;;; (nth $data 0)

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/Movies.csv")))
;;; (count $data)
;;; (nth $data 3)

(defn make-list-columns [column-names]
  (let [keys (map str (range 0 (count column-names)))
        entries (map (fn [k v] {"id" k "name" v "deleted" false})
                     keys column-names)]
    (zipmap keys entries)))

;;; (make-list-columns ["Title" "Star" "Director" "Costar" "Number"])

(defn load-csv-file [userid listid listname first-record-is-header pathname]
  (ensure-user-exists userid)
  (error-if (couchio/id-exists? (config/delectus-content-bucket) listid) "List ID in use"
            {:context 'load-csv-file :userid userid :listid listid})
  (error-if (model/list-name-exists? userid listname) "List name in use"
            {:context 'load-csv-file :userid userid :listname listname})
  (let [records (read-csv-file pathname)
        column-names (first records)
        columns (make-list-columns column-names)
        item-data (rest records)]
    (error-if-not (apply = (count column-names) (map count item-data))
                  "Some items have the wrong number of values"
                  {:context 'load-csv-file :column-names column-names})
    (let [item-docs (map #(model/values->item-document userid listid %) item-data)
          listdoc (model/make-list-document :id listid :owner userid :name listname :columns columns)]
      (model/assert-list! listdoc)
      (doseq [it item-docs]
        (model/assert-item! it)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $mikelid (model/email->userid "mikel@evins.net"))
;;; (def $listid (makeid))
;;; (def $listpath "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (model/list-name-exists? $mikelid "Zipcodes")
;;; (time (def $zipsdoc (load-csv-file $mikelid $listid "Zipcodes" true $listpath)))

;;; (def $mikelid (model/email->userid "mikel@evins.net"))
;;; (def $listid (makeid))
;;; (def $listpath "/home/mikel/Workshop/src/delectus/test-data/Movies.csv")
;;; (time (def $zipsdoc (load-csv-file $mikelid $listid "Movies" true $listpath)))

;;; (def $content-bucket (config/delectus-content-bucket))
;;; (def $items (couchio/find-objects $content-bucket :keys [] :limit nil :match {+type-key+ +item-type+}))
;;; (count $items)
;;; (def $lists (couchio/find-objects $content-bucket :keys [] :limit nil :match {+type-key+ +list-type+}))
;;; (count $lists)
;;; (def $collections (couchio/find-objects $content-bucket :keys [] :limit nil :match {+type-key+ +collection-type+}))
;;; (count $collections)
;;; (nth $items 0)

;;; (def $mikelid (model/email->userid "mikel@evins.net"))


(defn remove-documents! [bucket doclist]
  (doseq [doc doclist]
    (let [docid (.get doc +id-key+)]
      (if docid
        (couchio/remove-document! bucket docid)
        (throw (ex-info "Not an ID string"
                        {:context 'remove-documents!
                         :docid docid}))))))

;;; (def $content-bucket (config/delectus-content-bucket))
;;; (def $found (couchio/find-objects $content-bucket :keys [] :limit nil :match {+type-key+ +collection-type+}))
;;; (count $found)
;;; (remove-documents! $content-bucket $found)

