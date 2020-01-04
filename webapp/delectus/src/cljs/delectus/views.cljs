(ns delectus.views
  (:require
   [re-frame.core :as re-frame]
   [delectus.subs :as subs]
   ))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
        version (re-frame/subscribe [::subs/version])]
    [:nav {:class "navbar" "navbar-expand-md" "navbar-dark" "bg-dark" "fixed-top"}
     [:div {:class "container-fluid"}
      [:a {:class "navbar-brand" :href="#"} @name]
      [:span.version "version " @version]]]))

