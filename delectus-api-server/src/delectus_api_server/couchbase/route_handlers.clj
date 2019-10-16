(ns delectus-api-server.couchbase.route-handlers
  (:require [delectus-api-server.configuration :as config]))

;;; ---------------------------------------------------------------------
;;; Couchbase handlers
;;; ---------------------------------------------------------------------

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (config/couchbase-cluster)
               configuration (config/delectus-configuration)]
           (.authenticate couch
                          (:travel-sample-user configuration)
                          (:travel-sample-password configuration))
           (let [mgr (.clusterManager couch)
                 info (.raw (.info mgr))]
             (.toString info)))})
