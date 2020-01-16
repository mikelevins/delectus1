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

(defn login-form []
  [:div.loginbox
   [:form {:on-submit (fn [e](.preventDefault e))}
    [:input {:type :text :name :email :id "email_input"}]
    [:br]
    [:input {:type :password :name :password :id "password_input"}]
    [:br]
    [:button {:on-click  #(let [email (.-value (.getElementById js/document "email_input"))
                                password (.-value (.getElementById js/document "password_input"))]
                            (re-frame/dispatch [::events/post-login email password]))}
     "Log in"]]])

(defn logout-button []
  [:button {:on-click  #(let [auth (re-frame/subscribe [::subs/auth])
                              token (:token @auth)]
                          (re-frame/dispatch [::events/post-logout token]))}
   "Log out"])

;;; ---------------------------------------------------------------------
;;; main view
;;; ---------------------------------------------------------------------

(defn main-panel []
  (let [auth (re-frame/subscribe [::subs/auth])
        name (re-frame/subscribe [::subs/name])
        version (re-frame/subscribe [::subs/version])
        login-widget (login-form)]
    [:div [:nav {:class "navbar" "navbar-expand-md" "navbar-dark" "bg-dark" "fixed-top"}
           [:div {:class "container-fluid"}
            [:a {:class "navbar-brand" :href "#"} @name]
            [:span.version "version " @version]]]
     (if (:token @auth)
       [:div (logout-button)]
       [:div (login-form)])]))

