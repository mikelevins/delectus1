(ns delectus.views
  (:require
   [ajax.core :refer [GET]]
   [re-frame.core :as re-frame]
   [delectus.subs :as subs]
   ))

;;; ---------------------------------------------------------------------
;;; event handlers
;;; ---------------------------------------------------------------------

(re-frame/reg-event-db
 :get-motd
 (fn
   [db _]
   
   (GET  "http://localhost:3000/api/diagnostic/motd"
        {:handler       #(re-frame/dispatch [:process-motd %1])
         :error-handler #(re-frame/dispatch [:bad-motd-response %1])})
   
   ;; update a flag in `app-db` ... presumably to cause a "Loading..." UI 
   (assoc db :loading? true)))    ;; <3> return an updated db 

(re-frame/reg-event-db                   
 :process-motd             
 (fn
   [db [_ response]]           ;; destructure the response from the event vector
   (-> db
       (assoc :loading? false) ;; take away that "Loading ..." UI 
       (assoc :motd (js->clj response)))))

;;; ---------------------------------------------------------------------
;;; views
;;; ---------------------------------------------------------------------

(defn motd-button
  []
  [:button {:on-click  #(re-frame/dispatch [:get-motd])}  ;; get data from the server !!
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
