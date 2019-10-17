(ns delectus-api-server.couchbase.route-handlers
  (:require [delectus-api-server.configuration :as config]))

;;; ---------------------------------------------------------------------
;;; Couchbase handlers
;;; ---------------------------------------------------------------------

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [mgr (.clusterManager (config/couchbase-cluster)
                                    (:delectus-admin-user (config/delectus-configuration))
                                    (:delectus-admin-password (config/delectus-configuration)))
               info (.raw (.info mgr))]
           (.toString info))})
