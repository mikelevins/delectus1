(ns delectus.core
  (:require cljsjs.pouchdb
            [reagent.core :as reagent]
            [reagent-material-ui.core :as ui]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [delectus.utils :as utils]
            [delectus.mock :as mock]
            [delectus.db :as db]))

;;; ---------------------------------------------------------------------
;;; helpers
;;; ---------------------------------------------------------------------

(defn color [nm] (aget ui/colors nm))

;;; ---------------------------------------------------------------------
;;; Views
;;; ---------------------------------------------------------------------

(def +default-theme+
  {:muiTheme (clj->js (ui/getMuiTheme ui/lightBaseTheme))})

(defn home-page []
  [ui/MuiThemeProvider +default-theme+
   [:div
    [ui/AppBar {:title "Delectus"}]]])

;;; ---------------------------------------------------------------------
;;; Routes
;;; ---------------------------------------------------------------------

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

;;; ---------------------------------------------------------------------
;;; Initialize app
;;; ---------------------------------------------------------------------

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler (fn [path] (secretary/dispatch! path))
    :path-exists? (fn [path] (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

