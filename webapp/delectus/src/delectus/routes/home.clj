(ns delectus.routes.home
  (:require [compojure.core :refer :all]
            [delectus.views.layout :as layout]))

(defn home []
  (layout/common [:h1 "Delectus"]))

(defroutes home-routes
  (GET "/" [] (home)))
