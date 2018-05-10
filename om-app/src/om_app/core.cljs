(ns om-app.core
  (:require [goog.dom :as gdom]
            [om.dom :as dom]
            [om.next :as om :refer [defui]]))

(defui HelloWorld
  Object
  (render [this]
          (dom/div nil (get (om/props this) :title))))

(def hello (om/factory HelloWorld))

(js/ReactDOM.render
 (apply dom/div nil
        (map #(hello {:react-key %
                      :title (str "Hello " %)})
             (range 6)))
 (gdom/getElement "app"))

