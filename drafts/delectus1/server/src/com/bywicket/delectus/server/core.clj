(ns com.bywicket.delectus.server.core
  (:gen-class)
  (:use clojure.contrib.command-line))

(defn -main [& args]
  (with-command-line args
    "Delectus Web Services v. 1.0a1"
    [[port p "Delectus server control port" 11011]
     [conf c "Configuration file" "apiary.cf"]]
    (println "Delectus Web Services v. 1.0a1")))
