(ns ^:figwheel-no-load delectus.dev
  (:require
    [delectus.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
