(ns delectus.core
  (:require cljsjs.pouchdb
            [reagent.core :as reagent]
            [reagent-material-ui.core :as ui]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

;;; ---------------------------------------------------------------------
;;; local database
;;; ---------------------------------------------------------------------

;;; creates a local (client-side) database named "delectus"
(def +pouch+ (js/PouchDB. "delectus"))

;;; use the PouchDB API to get and print info about the created
;;; database:
;;; (in-ns 'delectus.core)
;;; (.then (.info +pouch+) (fn [obj] (println obj)))

;;; ---------------------------------------------------------------------
;;; helpers
;;; ---------------------------------------------------------------------

(defn color [nm] (aget ui/colors nm))

(def +mock-title+ "Fruits")
(def +mock-note+ "A list of potentially yummy fruits")
(def +mock-columns+ ["Name" "Color" "Distinction" "Count" "Delicious?"])
(def +mock-rows+ [["Apples" "red" "tangy" 3 "yes"]
                  ["Bananas" "yellow" "mushy" 4 "yes"]
                  ["Cherries" "red" "sweet" 35 "yes"]
                  ["Dates" "brown" "wrinkly" 22 "yes"]
                  ["Elephants" "gray" "awesome" 0 "no"]
                  ["Figs" "brown" "sweet" 0 "yes"]
                  ["Waffles" "tan" "buttery" 2 "yes"]])

;;; ---------------------------------------------------------------------
;;; Views
;;; ---------------------------------------------------------------------

(def +default-theme+
  (let [base-theme (ui/getMuiTheme ui/lightBaseTheme)
        theme-map (js->clj base-theme :keywordize-keys true)
        new-map (update theme-map
                        :palette merge
                        {:primary1Color (color "brown700")
                         :primary2Color (color "brown900")
                         :accent1Color (color "redA200")
                         :pickerHeaderColor (color "brown700")})]
    {:muiTheme (clj->js new-map)}))

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
   {:nav-handler
    (fn [path]
      (secretary/dispatch! path))
    :path-exists?
    (fn [path]
      (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

