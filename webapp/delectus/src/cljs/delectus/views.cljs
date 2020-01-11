(ns delectus.views
  (:require
   [re-frame.core :as re-frame]
   [delectus.events :as events]
   [delectus.subs :as subs]
   ))

;;; ---------------------------------------------------------------------
;;; views
;;; ---------------------------------------------------------------------

(defn motd-button
  []
  [:button {:on-click  #(re-frame/dispatch [::events/get-motd])}  ;; get data from the server !!
   "Message of the Day"])

(defn motd-plaque
  []
  (let [motd (re-frame/subscribe [::subs/motd])]
    [:span {:id "motd-display"} @motd]))

(defn motd-widget
  []
  [:div (motd-button)(motd-plaque)])

;;; ---------------------------------------------------------------------
;;; main view
;;; ---------------------------------------------------------------------

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
        version (re-frame/subscribe [::subs/version])]
    [:div [:nav {:class "navbar" "navbar-expand-md" "navbar-dark" "bg-dark" "fixed-top"}
           [:div {:class "container-fluid"}
            [:a {:class "navbar-brand" :href "#"} @name]
            [:span.version "version " @version]]]
     [:div (motd-widget)]]))

