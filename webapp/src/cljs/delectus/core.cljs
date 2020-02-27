(ns delectus.core
  (:require
   ;;[reagent.core :as r]
   ;;[re-frame.core :as rf]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType])
  (:import goog.History))

(defn home-page []
  [:section.section>div.container>div.content
   [:div
    [:p "Welcome to Delectus"]]])
