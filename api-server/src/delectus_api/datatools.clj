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

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/Movies.csv")))
;;; (count $data)
;;; (nth $data 1)

;;; (time (def $data (read-csv-file "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (count $data)
;;; (nth $data 1)

(defn load-csv-file [userid listid listname first-record-is-header pathname]
  (ensure-user-exists userid)
  (error-if (couchio/id-exists? (config/delectus-content-bucket) listid)
            "List ID in use"
            {:context 'load-csv-file
             :userid userid
             :listid listid})
  (error-if (model/list-name-exists? userid listname)
            "List name in use"
            {:context 'load-csv-file
             :userid userid
             :listname listname})
  (let [records (read-csv-file pathname)]
    records))

;;; (def $mikelid (model/email->userid "mikel@evins.net"))
;;; (time (def $movies (load-csv-file $mikelid "FOO" "My Movies" true "/home/mikel/Workshop/src/delectus/test-data/zipcode.csv")))
;;; (nth $movies 4)


;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $movies (make-list-document :name "Movies" :owner $mikelid))
;;; (ensure/ensure-document-type $movies +list-type+)
;;; (def $upserted-ls (assert-list! $movies))
