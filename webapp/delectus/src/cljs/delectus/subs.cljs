(ns delectus.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name (fn [db] (:name (:delectus db))))

(re-frame/reg-sub
 ::version (fn [db] (:version (:delectus db))))

(re-frame/reg-sub
 ::auth (fn [db] (:auth (:delectus db))))
