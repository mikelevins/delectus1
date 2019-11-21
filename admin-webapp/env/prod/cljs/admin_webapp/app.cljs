(ns admin-webapp.app
  (:require [admin-webapp.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
