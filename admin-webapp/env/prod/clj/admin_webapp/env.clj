(ns admin-webapp.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[admin-webapp started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[admin-webapp has shut down successfully]=-"))
   :middleware identity})
