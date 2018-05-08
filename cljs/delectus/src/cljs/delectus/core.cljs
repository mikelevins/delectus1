(ns delectus.core
  (:require [reagent.core :as reagent]
            [reagent-material-ui.core :as ui]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            ))

;; -------------------------
;; Views

(defonce +default-theme+
  {:muiTheme (ui/getMuiTheme ui/darkBaseTheme)})

(defn home-page []
  [ui/MuiThemeProvider +default-theme+
   [:div
    [ui/AppBar {:title "Delectus"}]
    [:h2 "Welcome"]
    [:div [:a {:href "/about"} "go to about page"]]]])

(defn about-page []
  [:div [:h2 "About delectus"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))


(secretary/defroute "/about" []
  (reset! page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (secretary/dispatch! path))
    :path-exists?
    (fn [path]
      (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
