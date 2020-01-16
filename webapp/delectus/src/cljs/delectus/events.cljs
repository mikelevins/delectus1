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
;;; post-login
;;; ---------------------------------------------------------------------

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
