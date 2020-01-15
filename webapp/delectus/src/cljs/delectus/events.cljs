(ns delectus.events
  (:require
   [ajax.core :refer [GET POST]]
   [re-frame.core :as re-frame]
   [delectus.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
   ))

;;; ---------------------------------------------------------------------
;;; initialize-db
;;; ---------------------------------------------------------------------

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _] db/default-db))

;;; ---------------------------------------------------------------------
;;; get-motd
;;; ---------------------------------------------------------------------

(re-frame/reg-event-db
 ::get-motd
 (fn
   [db _]
   
   (GET  "http://mars.local:3000/api/diagnostic/motd"
         {:handler       #(re-frame/dispatch [::process-motd %1])
          :error-handler #(re-frame/dispatch [::bad-motd-response %1])})
   
   ;; update a flag in `app-db` ... presumably to cause a "Loading..." UI 
   (assoc db :loading? true)))    ;; <3> return an updated db 

(re-frame/reg-event-db                   
 ::process-motd             
 (fn
   [db [_ response]]           ;; destructure the response from the event vector
   (-> db
       (assoc :loading? false) ;; take away that "Loading ..." UI 
       (assoc :motd (js->clj response)))))

;;; ---------------------------------------------------------------------
;;; post-login
;;; ---------------------------------------------------------------------

;; (POST "http://mars.local:3000/api/user/login"
;;       {:params
;;        {:email (.-value (.getElementById js/document "email_input"))
;;         :password (.-value (.getElementById js/document "password_input"))}
;;        :handler (fn [resp] (swap! auth (fn [] (merge @auth {:token (:token resp)}))))}))}


(re-frame/reg-event-db
 ::post-login
 (fn
   [db [_ email password]]
   (POST  "http://mars.local:3000/api/user/login"
          {:params {:email email :password password}
           :handler       #(re-frame/dispatch [::process-login %1])
           :error-handler #(re-frame/dispatch [::bad-login-response %1])})
   db))

(re-frame/reg-event-db                   
 ::process-login             
 (fn
   [db [_ response]]           ;; destructure the response from the event vector
   (assoc db :auth {:processed? true})))
