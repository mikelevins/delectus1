(ns delectus.views
  (:require
   [ajax.core :refer [GET POST]]
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [delectus.events :as events]
   [delectus.subs :as subs]))


;;; ---------------------------------------------------------------------
;;; views
;;; ---------------------------------------------------------------------

(defn motd-button []
  [:button {:on-click  #(re-frame/dispatch [::events/get-motd])}  ;; get data from the server !!
   "Message of the Day"])

(defn motd-plaque []
  (let [motd (re-frame/subscribe [::subs/motd])]
    [:span {:id "motd-display"} @motd]))

(defn motd-widget []
  [:div (motd-button)(motd-plaque)])

(def auth (reagent/atom {:token nil}))

(defn login-form []
  [:div.loginbox
   [:p "auth: " (str @auth)]
   [:form {:on-submit (fn [e](.preventDefault e))}
    [:input {:type :text :name :email :id "email_input"}]
    [:br]
    [:input {:type :password :name :password :id "password_input"}]
    [:br]
    [:button {:on-click  (fn []
                           (POST "http://localhost:3000/api/user/login"
                                 {:params
                                  {:email (.-value (.getElementById js/document "email_input"))
                                   :password (.-value (.getElementById js/document "password_input"))}
                                  :handler (fn [resp] (swap! auth (fn [] (merge @auth {:token (:token resp)}))))}))}
     "Log in"]]])

;;; ---------------------------------------------------------------------
;;; main view
;;; ---------------------------------------------------------------------

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
        version (re-frame/subscribe [::subs/version])
        login-widget (login-form)]
    [:div [:nav {:class "navbar" "navbar-expand-md" "navbar-dark" "bg-dark" "fixed-top"}
           [:div {:class "container-fluid"}
            [:a {:class "navbar-brand" :href "#"} @name]
            [:span.version "version " @version]]]
     [:div (motd-widget)]
     (if (:token @auth)
       [:div [:p "Auth: " (str @auth)]]
       [:div (login-form)])]))

