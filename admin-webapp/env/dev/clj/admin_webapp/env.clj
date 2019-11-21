(ns admin-webapp.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [admin-webapp.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[admin-webapp started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[admin-webapp has shut down successfully]=-"))
   :middleware wrap-dev})
