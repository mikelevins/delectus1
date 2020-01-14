(ns delectus.events
  (:require
   [ajax.core :refer [GET POST]]
   [re-frame.core :as re-frame]
   [delectus.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _] db/default-db))

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
