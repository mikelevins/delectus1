(defproject delectus-api "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/tools.logging "0.5.0"]
                 [metosin/compojure-api "2.0.0-alpha30"]
                 ;; config library
                 [aero "1.1.3"]
                 ;; auth library
                 [buddy/buddy-auth "2.2.0"]
                 ;; password-hashing library
                 [buddy/buddy-hashers "1.4.0"]
                 ;; Couchbase Java client
                 [com.couchbase.client/java-client "2.7.9"]
                 ;; html generation
                 [hiccup "1.0.5"]
                 ;; http server abstraction
                 [ring "1.8.0"]
                 [ring-cors "0.1.13"]
                 [ring/ring-json "0.5.0"]
                 ;; time utilities
                 [clj-time "0.15.2"]
                 ;; route debugging
                 [tupelo "0.9.175"]]
  :ring {:handler delectus-api.routes/app
         :nrepl {:start? true :port 22022}}
  :uberjar-name "delectus-api.jar"
  :profiles {:dev {:dependencies [[javax.servlet/javax.servlet-api "3.1.0"]]
                   :plugins [[lein-ring "0.12.5"]]}})

