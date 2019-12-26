(ns delectus-api.datatools
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [delectus-api.configuration :as config]
            [delectus-api.couchio :as couchio]
            [delectus-api.ensure :refer :all]
            [delectus-api.errors :refer :all]
            [delectus-api.identifiers :refer [makeid]]
            [delectus-api.model :as model]))

(defn read-csv-file [filename]
  (with-open [reader (io/reader filename)]
    (mapv identity (csv/read-csv reader))))

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (count $data)
;;; (nth $data 0)

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (count $data)
;;; (nth $data 1)

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
        item-data (rest records)
        listdoc (model/make-list-document :id listid :owner userid :name listname :columns columns)]
    listdoc))

;;; (def $mikelid (model/email->userid "mikel@evins.net"))
;;; (def $listid (makeid))
;;; (time (def $zipsdoc (load-csv-file $mikelid $listid "Zipcodes" true "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (def $zips (into {} (.toMap (.content $zipsdoc))))
;;; (get $zips "columns")
