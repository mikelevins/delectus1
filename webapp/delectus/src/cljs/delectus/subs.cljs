(ns delectus.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name (fn [db] (:name db)))

(re-frame/reg-sub
 ::version (fn [db] (:version db)))

(re-frame/reg-sub
 ::auth (fn [db] (:auth db)))
