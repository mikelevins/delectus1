(ns delectus.views
  (:require
   [re-frame.core :as re-frame]
   [delectus.subs :as subs]
   ))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
        version (re-frame/subscribe [::subs/version])]
    [:div
     [:h1 @name " "]
     [:p.version "version " @version]]))

